# Alternative SOAP server for Sympa

## Motivation

 - The current Sympa SOAP Server implementation is not SOAP 1.1 compliant, as it uses document/rpc encoding.
 - Authentification is very special in the current Sympa implementation (you need to login, first to get a cookie, which is anything else than WSE Security and needs special client implementations to support it. E.g. Axis2, Spring-WS or Apache CXF need heavy implimentation specific extensions to support this authentification). 
 - From a functional point of view, the current implementation is very basic and not suited to manage Sympa lists by Soap completely. 

## Goal
Due to these issues we decided to implement our own  Sympa SOAP implementation which is 
 - Soap 1.1 document/literal compliant.
 - Supports WSE compliant authentification scheme with UsernameToken WSSE headers.
 - Fits our functional needs to CRUD mailing lists with their admins and members.
 - Returns faults in case of errors.

## Installation

To install this module run the following commands:

```
cpanm --installdeps .
perl Build.PL
./Build
./Build  install
cp soap11-contrib/systemd/sympasoap11.service /usr/lib/systemd/system/
cp soap11-contrib/sysconfig/sympasoap11 /etc/sysconfig/
cp soap11-contrib/apache-httpd/sympasoap11.conf /etc/httpd/conf.d/
```
Please consult src/patches/README.md for probably needed patches to some perl modules.

## Debugging

```
	./Build clean
	./Build install
	/usr/bin/spawn-fcgi -n -F 1 -P /run/sympa/sympasoap11.pid -s /run/sympa/sympasoap11.socket -u sympa -g sympa -M 0600 -U apache -- /usr/libexec/sympa/sympasoap11.fcgi
```
