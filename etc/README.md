# Mac OS X router conf

Mac mini: public network (ISP) is on en0, private network is 10.0.0/24 statically configured on en1.
Command line:

```
sysctl net.inet.ip.forwarding=1
pfctl -f pf.conf -e
```

To have it at startup, insert the content of [pf.conf](pf.conf) into the nat section of /etc/pf.conf
and [sysctl.conf](sysctl.conf) to `/etc/sysctl.conf`