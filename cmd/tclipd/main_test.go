package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestAuthKeyFromEnv(t *testing.T) {
	// writeKeyFile writes contents to a temp file and returns its path.
	writeKeyFile := func(t *testing.T, contents string) string {
		t.Helper()
		path := filepath.Join(t.TempDir(), "authkey")
		if err := os.WriteFile(path, []byte(contents), 0600); err != nil {
			t.Fatal(err)
		}
		return path
	}

	t.Run("unset", func(t *testing.T) {
		t.Setenv("TS_AUTHKEY", "")

		got, err := authKeyFromEnv()
		if err != nil {
			t.Fatalf("authKeyFromEnv() = %v", err)
		}
		if got != "" {
			t.Errorf("got %q, want empty so tsnet falls back to interactive login", got)
		}
	})

	t.Run("literal key", func(t *testing.T) {
		t.Setenv("TS_AUTHKEY", "tskey-auth-literal")

		got, err := authKeyFromEnv()
		if err != nil {
			t.Fatalf("authKeyFromEnv() = %v", err)
		}
		if want := "tskey-auth-literal"; got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})

	t.Run("file", func(t *testing.T) {
		t.Setenv("TS_AUTHKEY", "file:"+writeKeyFile(t, "tskey-auth-fromfile"))

		got, err := authKeyFromEnv()
		if err != nil {
			t.Fatalf("authKeyFromEnv() = %v", err)
		}
		if want := "tskey-auth-fromfile"; got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})

	t.Run("file with trailing newline", func(t *testing.T) {
		t.Setenv("TS_AUTHKEY", "file:"+writeKeyFile(t, "tskey-auth-fromfile\n"))

		got, err := authKeyFromEnv()
		if err != nil {
			t.Fatalf("authKeyFromEnv() = %v", err)
		}
		if want := "tskey-auth-fromfile"; got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})

	// A misconfigured key file must be a hard error rather than silently
	// falling through to an interactive login the operator will never see.
	errorCases := map[string]string{
		"missing file": "file:" + filepath.Join(t.TempDir(), "does-not-exist"),
		"empty file":   "file:" + writeKeyFile(t, ""),
		"blank file":   "file:" + writeKeyFile(t, "\n\n"),
		"no path":      "file:",
	}
	for name, value := range errorCases {
		t.Run(name, func(t *testing.T) {
			t.Setenv("TS_AUTHKEY", value)

			got, err := authKeyFromEnv()
			if err == nil {
				t.Fatalf("authKeyFromEnv() = %q, want error", got)
			}
		})
	}
}
