package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"
)

var (
	fname  = flag.String("n", "", "filename to use for pastes from standard input")
	target = flag.String("target", "http://paste", "tclip server URL")
)

func main() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [filename]\n\nTakes a file or standard in and posts it to your tailnet's tclip service. Returns a URL on standard out for sharing.\n\nFlags:\n", filepath.Base(os.Args[0]))
		flag.PrintDefaults()
		os.Exit(2)
	}
	flag.Parse()

	fin := os.Stdin
	switch flag.NArg() {
	case 0:
		if *fname == "" {
			*fname = "paste.txt"
		}
	case 1:
		var err error
		fin, err = os.Open(flag.Arg(0))
		if err != nil {
			log.Fatal(err)
		}
		defer fin.Close()

		*fname = flag.Arg(0)
	default:
		flag.Usage()
	}

	u, err := url.Parse(*target)
	if err != nil {
		log.Fatalf("%q is not a legal url: %v", *target, err)
	}

	u.Path = "/api/post"

	data, err := io.ReadAll(fin)
	if err != nil {
		log.Fatalf("can't read data: %v", err)
	}

	q := url.Values{}

	q.Set("filename", filepath.Base(*fname))
	q.Set("content", string(data))

	req, err := http.NewRequest(http.MethodPost, u.String(), strings.NewReader(q.Encode()))
	if err != nil {
		log.Fatalf("can't make HTTP request: %v", err)
	}

	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.Header.Set("Accept", "text/plain")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Fatalf("can't post to %s: %v", u, err)
	}

	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		body, err := io.ReadAll(resp.Body)
		if err != nil {
			log.Printf("[unexpected] can't read from body of HTTP response: %v", err)
		}
		log.Fatalf("error submitting paste: %s", string(body))
	}

	io.Copy(os.Stdout, resp.Body)
	fmt.Printf("\n")
}
