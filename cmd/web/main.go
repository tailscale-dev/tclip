package main

import (
	"context"
	"crypto/md5"
	"database/sql"
	"database/sql/driver"
	_ "embed"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"github.com/google/uuid"
	"github.com/tailscale/sqlite"
	"tailscale.com/client/tailscale"
	"tailscale.com/client/tailscale/apitype"
	"tailscale.com/tailcfg"
	"tailscale.com/tsnet"
)

var (
	hostname = flag.String("hostname", envOr("TSNET_HOSTNAME", "paste"), "hostname to use on your tailnet, TSNET_HOSTNAME in the environment")
	dataDir  = flag.String("data-location", dataLocation(), "where data is stored, defaults to DATA_DIR or ~/.config/tailscale/paste")

	//go:embed schema.sql
	sqlSchema string
)

const formDataLimit = 64 * 1024 // 64 kilobytes (approx. 32 printed pages of text)

func dataLocation() string {
	dir, err := os.UserConfigDir()
	if err != nil {
		return os.Getenv("DATA_DIR")
	}
	return filepath.Join(dir, "tailscale", "paste")
}

func envOr(key, defaultVal string) string {
	if result, ok := os.LookupEnv(key); ok {
		return result
	}
	return defaultVal
}

func main() {
	flag.Parse()

	os.MkdirAll(*dataDir, 0700)
	os.MkdirAll(filepath.Join(*dataDir, "tsnet"), 0700)

	s := &tsnet.Server{
		Hostname: *hostname,
		Dir:      filepath.Join(*dataDir, "tsnet"),
		Logf:     func(string, ...any) {},
	}

	if err := s.Start(); err != nil {
		log.Fatal(err)
	}

	db, err := openDB(*dataDir)
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	lc, err := s.LocalClient()
	if err != nil {
		log.Fatal(err)
	}
	_ = lc

	ln, err := s.Listen("tcp", ":80")
	if err != nil {
		log.Fatal(err)
	}

	mux := http.NewServeMux()

	mux.HandleFunc("/paste/", func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodGet {
			http.Error(w, "must GET", http.StatusMethodNotAllowed)
			return
		}
		
		sp := strings.Split(r.URL.Path, "/")
		sp = sp[2:]

		if len(sp) == 0 {
			http.Redirect(w, r, "/", http.StatusTemporaryRedirect)
			return
		}

		id := sp[0]

		q := `
SELECT p.filename
     , p.data
     , u.id
     , u.login_name
     , u.display_name
     , u.profile_pic_url
FROM pastes p
INNER JOIN users u
  ON p.user_id = u.id
WHERE p.id = ?1`

		row := db.QueryRowContext(r.Context(), q, id)
		var fname, data, userID, userLoginName, userDisplayName, userProfilePicURL string

		err := row.Scan(&fname, &data, &userID, &userLoginName, &userDisplayName, &userProfilePicURL)
		if err != nil {
			log.Printf("%s: looking up %s: %v", r.RemoteAddr, id, err)
			http.Error(w, fmt.Sprintf("can't find paste %s: %v", id, err), http.StatusInternalServerError)
			return
		}

		if len(sp) != 1 && sp[1] == "raw" {
			w.Header().Set("Content-Type", "text/plain; charset=utf-8")
			w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=%q", fname))
			w.Header().Set("Content-Length", fmt.Sprintf("%d", len(data)))

			w.WriteHeader(http.StatusOK)
			fmt.Fprint(w, data)
			return
		}

		http.Error(w, "not implemented yet", http.StatusNotImplemented)
	})

	mux.HandleFunc("/api/post", func(w http.ResponseWriter, r *http.Request) {
		userInfo, err := upsertUserInfo(r.Context(), db, lc, r.RemoteAddr)
		if err != nil {
			log.Printf("%s: %v", r.RemoteAddr, err)
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		if strings.HasPrefix(r.Header.Get("Content-Type"), "multipart/form-data;") {
			err = r.ParseMultipartForm(formDataLimit)
		} else if r.Header.Get("Content-Type") == "application/x-www-form-urlencoded" {
			err = r.ParseForm()
		} else {
			log.Printf("%s: unknown content type: %s", r.RemoteAddr, r.Header.Get("Content-Type"))
			http.Error(w, "bad content-type, should be a form", http.StatusBadRequest)
			return
		}
		if err != nil {
			log.Printf("%s: bad form: %v", r.RemoteAddr, err)
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		if !r.Form.Has("filename") && !r.Form.Has("data") {
			log.Printf("%s: posted form without filename and data", r.RemoteAddr)
			http.Error(w, "include form values filename and data", http.StatusBadRequest)
			return
		}

		fname := r.Form.Get("filename")
		data := r.Form.Get("data")
		id := uuid.NewString()

		q := `
INSERT INTO pastes
    ( id
    , user_id
    , filename
    , data
    )
VALUES
    ( ?1
    , ?2
    , ?3
    , ?4
    )`

		_, err = db.ExecContext(
			r.Context(),
			q,
			id,
			userInfo.UserProfile.ID,
			fname,
			data,
		)
		if err != nil {
			log.Printf("%s: %v", r.RemoteAddr, err)
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		log.Printf("new paste: %s", id)

		w.WriteHeader(http.StatusOK)
		fmt.Fprintf(w, "http://%s/paste/%s", r.Host, id)
	})

	log.Printf("listening on http://%s", *hostname)
	log.Fatal(http.Serve(ln, mux))
}

func openDB(dir string) (*sql.DB, error) {
	db := sql.OpenDB(sqlite.Connector("file:"+filepath.Join(dir, "data.db"), func(ctx context.Context, conn driver.ConnPrepareContext) error {
		return sqlite.ExecScript(conn.(sqlite.SQLConn), sqlSchema)
	}, nil))

	err := db.Ping()
	if err != nil {
		return nil, err
	}

	return db, nil
}

func md5Hash(inp string) string {
	h := md5.New()
	return fmt.Sprintf("%x", h.Sum([]byte(inp)))
}

func upsertUserInfo(ctx context.Context, db *sql.DB, lc *tailscale.LocalClient, remoteAddr string) (*apitype.WhoIsResponse, error) {
	userInfo, err := lc.WhoIs(ctx, remoteAddr)
	if err != nil {
		return nil, err
	}

	if userInfo.UserProfile.LoginName == "tagged-devices" {
		userInfo.UserProfile.ID = tailcfg.UserID(userInfo.Node.ID)
		userInfo.UserProfile.LoginName = userInfo.Node.Hostinfo.Hostname()
		userInfo.UserProfile.DisplayName = fmt.Sprintf("tagged node %s: %s", userInfo.Node.Hostinfo.Hostname(), userInfo.Node.Tags[0])
		userInfo.UserProfile.ProfilePicURL = fmt.Sprintf("https://www.gravatar.com/avatar/%s", md5Hash(userInfo.Node.ComputedNameWithHost))
	}

	q := `
INSERT INTO users
    ( id
    , login_name
    , display_name
    , profile_pic_url
    )
VALUES
    ( ?1
    , ?2
    , ?3
    , ?4
    )
ON CONFLICT DO
  UPDATE SET
      login_name =      ?2
    , display_name =    ?3
    , profile_pic_url = ?4
    `

	_, err = db.ExecContext(
		ctx,
		q,
		userInfo.UserProfile.ID,
		userInfo.UserProfile.LoginName,
		userInfo.UserProfile.DisplayName,
		userInfo.UserProfile.ProfilePicURL,
	)
	if err != nil {
		return nil, err
	}

	return userInfo, nil
}
