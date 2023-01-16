# infinipaste

[![status: experimental](https://img.shields.io/badge/status-experimental-blue)](https://tailscale.com/kb/1167/release-stages/#experimental)

A self-hostable pastebin for your tailnet. It can store an infinite\*
number of pastes.

## Development

1. Install Nix
2. Enable flakes
3. `nix develop`

You can then test your changes to infinipaste by running `go run
./cmd/web` or `go run ./cmd/infinipaste` as appropriate.

## Building for prod

The web server:
```
nix build .#web
```

The docker image:
```
nix build .#docker
docker load < ./result
```

The portable service image:
```
nix build .#portable-service
```

## Deploying

There are several options you can take to deploy infinipaste. The most
basic deployment method is to use [fly.io](https://fly.io), but we
offer a few options so that you can make the decision that is right
for you.

### Prerequisites

In order to deploy this service, you will need to get an authkey from
[the admin panel](https://login.tailscale.com/admin/settings/keys). It
is a good idea to associate this with the tag `tag:service` or its own
`tag:infinipaste`. Please also do not set the `ephemeral` tag as that
will destroy the node when the service shuts down.

Your authkey should start with `tskey-auth`.

You will need to have Magic DNS enabled.

You will need to have 

### fly.io

In one of your infrastructure management GitHub repositories, create a
folder for infinipaste and then copy the following
[`fly.toml`](https://fly.io/docs/reference/configuration/) template
into that folder.

```toml
app = "FLY_APP_NAME"

[build]
image = "ghcr.io/tailscale-dev/infinipaste:main"

[deploy]
strategy = "immediate"

[env]
DATA_DIR = "/data"

[mounts]
source = "infinipaste_data"
destination = "/data"
```

Replace `FLY_APP_NAME` with a name such as `yourorg-infinipaste` and
then run these commands with [the `flyctl` command](https://fly.io/docs/hands-on/install-flyctl/):

```console
$ flyctl apps create FLY_APP_NAME
$ flyctl volumes create infinipaste_data
$ flyctl secrets set TS_AUTHKEY=<key>
$ flyctl deploy
```

You should be able to open the app at [http://paste](http://paste) and
paste to your heart's content.

### Normal Docker

To run this service in Docker, run the following command:

```
docker run \
  -d \
  -v /var/lib/infinipaste:/data \
  -e DATA_DIR=/data \
  -e TS_AUTHKEY=<key> \
  -n infinipaste \
  --restart always \
  ghcr.io/tailscale-dev/infinipaste:main
```

Every so often you should pull a new version of infinipaste and
recreate the container:

```
docker pull ghcr.io/tailscale-dev/infinipaste:main
docker rm -f infinipaste
```

Then run the above command to recreate the container.

#### Backups

Add the path `/var/lib/infinipaste` to your backup program of choice.

### Systemd portable service

systemd has a mechanism for [portable
service](https://systemd.io/PORTABLE_SERVICES/) which are like Docker
images mixed with systemd unit configurations. This allows you to
install infinipaste on any Linux distribution with systemd (and the
`portablectl` command, install `systemd-container` to get this package
on Ubuntu). This lets you view infinipaste logs with `journalctl` and
manage it like any other systemd service.

Download the portable service image yada yada TODO(Xe) fix this

```console
TODO(Xe): this
```

Then run `portablectl list` to get a list of available portable
services:

```console
$ portablectl list
NAME                       TYPE RO  CRTIME                      MTIME                       USAGE STATE
infinipaste_0.1.0-20230116 raw  yes Mon 2023-01-16 16:49:58 UTC Mon 2023-01-16 16:53:59 UTC 14.8M detached
```

Copy the name of the `infinipaste` service including the
auto-generated version number to your clipboard (for example:
`infinipaste_0.1.0-20230116`) and pass that to `portablectl attach`:

```console
$ sudo portablectl attach infinipaste_0.1.0-20230116
```

Next, create the folder `/etc/systemd/system/infinipaste.service.d`
and create the file `10-ts-auth-key.conf` in it with the following contents:

```systemd
# /etc/systemd/system/infinipaste.service.d/10-ts-auth-key.conf
[Service]
Environment=TS_AUTH_KEY=<key>
```

Finally, enable `infinipaste.service` and start it with `systemctl
enable --now`:

```console
$ sudo systemctl enable --now infinipaste.service
```

Wait a moment for it to connect to Tailscale and then check on it with
`tailscale status`. Your new node named `paste` should show up in your
tailnet.

#### Backups

Add the path `/var/lib/private/infinipaste` to your backup program of
choice.
