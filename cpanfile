# Perl module requirements

#feature 'soap11', 'Required if you want to run the SOAP 1.1 / WSSE compliant soap web service.' => sub {

requires 'XML::Compile', '>= 0';
requires 'XML::Compile::SOAP11', '>= 3.27';
requires 'XML::Compile::WSDL11', '>= 0';
requires 'XML::Compile::SOAP::Daemon::CGI', '>= 0';
requires 'XML::Compile::WSS::Util', '>= 0';

#};
