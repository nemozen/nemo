# Mac OS X router conf

Tested on Mac mini with Darwin 19.6.0. 

Public network (ISP) is on en1, private network is 10.0.0/24 on en0.
Command line:

```
sudo ifconfig en0 10.0.0.1 255.255.255.0
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
