# tclip

[![status: experimental](https://img.shields.io/badge/status-experimental-blue)](https://tailscale.com/kb/1167/release-stages/#experimental)

A self-hostable pastebin for your tailnet. It can store an infinite\*
number of pastes.

## Development

1. Install Nix
2. Enable flakes
3. `nix develop`

You can then test your changes to tclip by running `go run
./cmd/tclipd` or `go run ./cmd/tclip` as appropriate.

Note that for the first run of `./cmd/tclipd`, you *must* set
either the `TS_AUTHKEY` environment variable, or run it with
`--tsnet-verbose` to get the login URL for Tailscale.

## Building for prod

The web server:
```
nix build .#tclipd
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

There are several options you can take to deploy tclip. The most
basic deployment method is to use [fly.io](https://fly.io), but we
offer a few options so that you can make the decision that is right
for you.

### Prerequisites

In order to deploy this service, you will need to get an authkey from
[the admin panel](https://login.tailscale.com/admin/settings/keys). It
is a good idea to associate this with the tag `tag:service` or its own
`tag:tclip`. Please also do not set the `ephemeral` tag as that
will destroy the node when the service shuts down.

Your authkey should start with `tskey-auth`.

You will need to have Magic DNS enabled.

### fly.io

In one of your infrastructure management GitHub repositories, create a
folder for tclip and then copy the following
[`fly.toml`](https://fly.io/docs/reference/configuration/) template
into that folder.

```toml
app = "FLY_APP_NAME"

[build]
image = "ghcr.io/tailscale-dev/tclip:latest"

[deploy]
strategy = "immediate"

[env]
DATA_DIR = "/data"

[mounts]
source = "tclip_data"
destination = "/data"
```

Replace `FLY_APP_NAME` with a name such as `yourorg-tclip` and
then run these commands with [the `flyctl` command](https://fly.io/docs/hands-on/install-flyctl/):

```console
$ flyctl apps create FLY_APP_NAME
$ flyctl volumes create tclip_data
$ flyctl secrets set TS_AUTHKEY=<key>
$ flyctl deploy
```

You should be able to open the app at [http://paste](http://paste) and
paste to your heart's content.

#### Updating

Run `flyctl deploy` to update the service.

### Normal Docker

To run this service in Docker, run the following command:

```
docker run \
  -d \
  -v /var/lib/tclip:/data \
  -e DATA_DIR=/data \
  -e TS_AUTHKEY=<key> \
  -n tclip \
  --restart always \
  ghcr.io/tailscale-dev/tclip:latest
```

#### Updating

Every so often you should pull a new version of tclip and
recreate the container:

```
docker pull ghcr.io/tailscale-dev/tclip:latest
docker rm -f tclip
```

Then run the above command to recreate the container.

#### Backups

Add the path `/var/lib/tclip` to your backup program of choice.

### Systemd portable service

systemd has a mechanism for [portable
service](https://systemd.io/PORTABLE_SERVICES/) which are like Docker
images mixed with systemd unit configurations. This allows you to
install tclip on any Linux distribution with systemd (and the
`portablectl` command, install `systemd-container` to get this package
on Ubuntu). This lets you view tclip logs with `journalctl` and
manage it like any other systemd service.

You can download the portable service image from CI by looking at the
[recently finished
builds](https://github.com/tailscale-dev/tclip/actions?query=is%3Asuccess+branch%3Amain),
clicking on the most recent one, and downloading the
`portable-service` artifact. This will get you a zipfile that contains
a single `.raw` file. Copy this `.raw` file to `/var/lib/portable` on
your target server. Then run `portablectl list` to get a list of
available portable services:

```console
$ portablectl list
NAME                       TYPE RO  CRTIME                      MTIME                       USAGE STATE
tclip_0.1.0-20230116 raw  yes Mon 2023-01-16 16:49:58 UTC Mon 2023-01-16 16:53:59 UTC 14.8M detached
```

Copy the name of the `tclip` service including the
auto-generated version number to your clipboard (for example:
`tclip_0.1.0-20230116`) and pass that to `portablectl attach`:

```console
$ sudo portablectl attach tclip_0.1.0-20230116
```

Next, create the folder `/etc/systemd/system/tclip.service.d`
and create the file `10-ts-auth-key.conf` in it with the following
contents (be sure to replace `<key>` with your tailnet authkey):

```systemd
# /etc/systemd/system/tclip.service.d/10-ts-authkey.conf
[Service]
Environment=TS_AUTHKEY=<key>
```

Finally, enable `tclip.service` and start it with `systemctl
enable --now`:

```console
$ sudo systemctl enable --now tclip.service
```

Wait a moment for it to connect to Tailscale and then check on it with
`tailscale status`. Your new node named `paste` should show up in your
tailnet.

#### Updating

To update tclip, first detach the portable service using `portablectl
detach` after finding the list with `portablectl list`. Then delete
the correlating `.raw` file in `/var/lib/portables`. Download a new
one in its place and re-attach and re-enable the service.

#### Backups

Add the path `/var/lib/private/tclip` to your backup program of
choice.
