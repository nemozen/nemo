# Mac OS X router conf

Mac mini: public network (ISP) is on en0, private network is 10.0.0/24 on en1.
Command line:

```
sudo sysctl net.inet.ip.forwarding=1						# IP forwarding
sudo pfctl -f pf.conf -e							# NAT
sudo /bin/launchctl load -w /System/Library/LaunchDaemons/bootps.plist	        # DHCP (with /etc/bootpd.list below)
```

To have it at startup, copy these files in to `/etc/`

- [pf.conf](pf.conf) 
  - if there's an existing pf.conf include this in it right after existing nat-anchor statement
  - pf is disabled by default, so you have to
    - reboot into recovery mode and `csrutil disable`
    - modify `/System/Library/LaunchDaemons/com.apple.pfctl.plist` and [add `-e` to `ProgramArguments`](https://apple.stackexchange.com/questions/308182/how-to-launch-pf-at-startup) 
- [sysctl.conf](sysctl.conf)
- [bootpd.plist](bootpd.plist)


# VPN tunnelling

With Ubuntu server as public end of VPN tunnel, Mac OS router to private LAN

## Local (macos)

Basic router setup as above, but no need for NAT on client router:

```
pfctl -F nat
```

Client  `/usr/local/etc/wireguard/utun1.conf`

```
[Interface]
PrivateKey = ***
Address = 192.168.6.2/32
DNS = 1.1.1.1
MTU = 1420
 
[Peer]
PublicKey = ***
Endpoint = 23.20.210.201:41194
AllowedIPs = 0.0.0.0/0, ::0
PersistentKeepalive = 5
```

Turning on
```
sudo wg-quick up utun1
```

## Remote (ubuntu)

$ sudo apt-get install wireguard-tools
$ (umask 0077; wg genkey > private.key)
$ wg pubkey < private.key > public.key
$ sudo vi /etc/wireguard/wg0.conf

Content of /etc/wireguard/wg0.conf
```
[Interface]
Address = 192.168.6.1/32
ListenPort = 41194
PrivateKey = ***
MTU = 1200

[Peer]
PublicKey = ***
AllowedIPs = 192.168.6.2/32, 10.0.0.0/24
```

Enable 
```
$ sudo systemctl enable wg-quick@wg0
$ sudo systemctl start wg-quick@wg0
$ sudo systemctl status wg-quick@wg0
```

Edit AWS security group to allow UDP in to server

Enable ip forwarding
```
$ sudo sysctl -w net.ipv4.ip_forward=1
$ sudo sysctl -w net.ipv6.conf.all.forwarding=1
$ sudo ufw allow 41194/udp
```
Enable ip NAT (external interface is ens5)
```
$ sudo iptables -A FORWARD -i %i -j ACCEPT; sudo iptables -A FORWARD -o %i -j ACCEPT; sudo iptables -t nat -A POSTROUTING -o ens5 -j MASQUERADE
```

To disable, shutdown wg interface and disable  NAT
```
sudo wg-quick down wg0
$ sudo iptables -D FORWARD -i %i -j ACCEPT; sudo iptables -D FORWARD -o %i -j ACCEPT; sudo iptables -t nat -D POSTROUTING -o ens5 -j MASQUERADE
```
