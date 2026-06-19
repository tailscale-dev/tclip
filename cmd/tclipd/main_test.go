package main

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/google/uuid"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"tailscale.com/client/tailscale/apitype"
	"tailscale.com/tailcfg"
)

type testClient struct {
	who apitype.WhoIsResponse
}

func (tc *testClient) WhoIs(ctx context.Context, remoteAddr string) (*apitype.WhoIsResponse, error) {
	return &tc.who, nil
}

func TestShowPost(t *testing.T) {
	tests := []struct {
		name       string
		capmap     tailcfg.PeerCapMap
		wantStatus int
	}{
		{
			name: "no-caps",
			capmap: tailcfg.PeerCapMap{
				capName: []tailcfg.RawMessage{
					``,
				},
			},
			wantStatus: 401,
		},
		{
			name: "admin-all-access",
			capmap: tailcfg.PeerCapMap{
				capName: []tailcfg.RawMessage{
					`{"all": {"admin": true}}`,
				},
			},
			wantStatus: 200,
		},
		{
			name: "read-all",
			capmap: tailcfg.PeerCapMap{
				capName: []tailcfg.RawMessage{
					`{"all": {"read": true}}`,
				},
			},
			wantStatus: 200,
		},
		{
			name: "all-list",
			capmap: tailcfg.PeerCapMap{
				capName: []tailcfg.RawMessage{
					`{"all": {"list": true}}`,
				},
			},
			wantStatus: 200,
		},
		{
			name: "user-read",
			capmap: tailcfg.PeerCapMap{
				capName: []tailcfg.RawMessage{
					`{"user": {"read": true}}`,
				},
			},
			wantStatus: 200,
		},
		{
			name: "user-no-read",
			capmap: tailcfg.PeerCapMap{
				capName: []tailcfg.RawMessage{
					`{"user": {"read": false}}`,
				},
			},
			wantStatus: 401,
		},
		{
			name: "user-write",
			capmap: tailcfg.PeerCapMap{
				capName: []tailcfg.RawMessage{
					`{"user": {"write": true}}`,
				},
			},
			wantStatus: 200,
		},
		{
			name: "user-list",
			capmap: tailcfg.PeerCapMap{
				capName: []tailcfg.RawMessage{
					`{"user": {"list": true}}`,
				},
			},
			wantStatus: 200,
		},
		{
			name: "all-read-override",
			capmap: tailcfg.PeerCapMap{
				capName: []tailcfg.RawMessage{
					`{"user": {"read": false}}`,
					`{"all": {"admin": true}}`,
				},
			},
			wantStatus: 200,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			dataDir := t.TempDir()
			db, err := openDB(dataDir)
			require.NoError(t, err)
			tc := testClient{who: apitype.WhoIsResponse{
				UserProfile: &tailcfg.UserProfile{
					ID:          1,
					LoginName:   "test@example.com",
					DisplayName: "Test",
				},
				Node:   &tailcfg.Node{},
				CapMap: tt.capmap,
			}}
			s := NewServer(&tc, db, "")

			id := uuid.NewString()
			err = s.CreatePaste(t.Context(), id, time.Now(), 1, "test.txt", "tomato")
			assert.NoError(t, err)

			req := httptest.NewRequest(http.MethodGet, fmt.Sprintf("/paste/%s", id), nil)
			w := httptest.NewRecorder()
			s.ShowPost(w, req)
			res := w.Result()
			defer res.Body.Close()

			assert.Equal(t, tt.wantStatus, res.StatusCode)

			if tt.wantStatus == http.StatusOK {
				data, err := io.ReadAll(res.Body)
				require.NoError(t, err)
				assert.Contains(t, string(data), "tomato")
			}
		})
	}
}
