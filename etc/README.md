# Mac OS X router conf

Mac mini: public network (ISP) is on en0, private network is 10.0.0/24 on en1.
Command line:

```
sysctl net.inet.ip.forwarding=1						# IP forwarding
pfctl -f pf.conf -e							# NAT
sudo /bin/launchctl load -w /System/Library/LaunchDaemons/bootps.plist	# DHCP
```

To have it at startup, copy these files in to `/etc/`

- [pf.conf](pf.conf) (if there's an existing pf.conf insert the contents right after existing nat-anchor statement)
- [sysctl.conf](sysctl.conf)
- [boopd.plist](bootpd.plist)