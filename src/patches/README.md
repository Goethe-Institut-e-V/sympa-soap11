# Perl modules patches

Unfortunately I did encounter problems with some perl modules. Patches are provided here, modules maintainers are informed.

## XML::Compile::SOAP

https://metacpan.org/dist/XML-Compile-SOAP/view/lib/XML/Compile/SOAP.pod

### CGI.pm.patch

Allow option to set "nph" to 0.

```
patch -b -d [PathToModlues]/XML/Compile/SOAP/Daemon < [PathToSource]/src/patch/CGI.pm.patch
```
```
patch -b -d /opt/perl/perls/perl-5.30.3/lib/site_perl/5.30.3/XML/Compile/SOAP/Daemon < CGI.pm.patch~~
```


### Server.pm.patch

Since XML-Compile-SOAP-3.27 this is fixed!

~~Some newer version introduced a bug where xml send by a client not fitting the wsdl crashed the application.~~

```
patch -b -d [PathToModlues]/XML/Compile/SOAP < [PathToSource]/src/patch/Server.pm.patch
```
