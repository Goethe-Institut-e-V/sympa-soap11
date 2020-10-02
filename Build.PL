use strict;
use warnings FATAL => 'all';

use Module::Build;

# default build configuration from (centos) rpm distribution
my $d_sympa_moduledir = '/usr/share/sympa/lib';
my $d_build_with_unitsdir = '/usr/lib/systemd/system';


my $build = Module::Build->new(
    module_name        => 'SOAP11',
    dist_name          => 'SympaSOAP11',
    license            => 'gpl3',
    dist_author        => q{Immo Goltz <immo.goltz@goethe.de>},
    dist_version_from  => 'lib/SOAP11.pm',
    dist_abstract      => 'SOAP11 compliant interface for Sympa',
    release_status     => 'testing',
    dynamic_config => 1,
    create_packlist => 0,
    #create_makefile_pl => 'traditional',
    configure_requires => {
        'Module::Build' => 0,
    },
    build_requires => {
        'Test::More' => 0,
    },
    requires => {
        'XML::Compile'                    => 0,
        'XML::Compile::SOAP11'            => 0,
        'XML::Compile::WSDL11'            => 0,
        'XML::Compile::SOAP::Daemon::CGI' => 3.09,
        'XML::Compile::WSS::Util'         => 0,
    },
    add_to_cleanup => [
        'blib',
        'dist',
    ],
    needs_compiler => 0,
    PL_files => {
        'src/make.PL' => ['src/sympasoap11.fcgi', 'src/sympasoap11.service', 'src/sympasoap11.conf', 'src/sympasoap11'],
    },
    script_files   => {
       'src/sympasoap11.fcgi' => 'bin/sympasoap11.fcgi',
    },
    wsdl_files   => {
       'lib/SOAP11/sympasoap11.xsd' => 'lib/SOAP11/sympasoap11.xsd',
       'lib/SOAP11/sympasoap11.wsdl' => 'lib/SOAP11/sympasoap11.wsdl',
    },
    other_files => {
        'src/sympasoap11.service' => 'contrib/systemd/sympasoap11.service',
        'src/sympasoap11' => 'contrib/sysconfig/sympasoap11',
        'src/sympasoap11.conf' => 'contrib/apache-httpd/sympasoap11.conf',
    },
    get_options    => {
    	'help' => {
    		default => 0,
    		},
    	'h' => {
    		default => 0,
    		},
        # Sympa lib installation
        'sympa-moduledir' => {
            type    => '=s',
            default => $d_sympa_moduledir,
        },
        # systems systemd units
        'with-unitsdir' => {
            type    => '=s',
            default => $d_build_with_unitsdir,
        },
    },
);

# Sympa lib taget subdirectory
my $my_sympa_modulesubdir = '/Sympa/WWW';
my $sympa_moduledir =  $build->args('sympa-moduledir');
my $system_unitsdir =  $build->args('with-unitsdir');

print "Checking for Sympa in $sympa_moduledir...\n";
my $check = eval {
    require "$sympa_moduledir/Sympa/Constants.pm";
    Sympa::Constants->import();
    1;
};
if ($check) {
    print "Found Sympa version " . Sympa::Constants->VERSION . " in $sympa_moduledir.\n";
    print "Importing configuration...\n";
} else {
    print "Sympa not found in $sympa_moduledir.\n";
    print "Specify correct path, see usage.\n";
}


if ( $build->args('help') || $build->args('h') || !$check ) {
print <<END;

'Build.PL' configures SympaSOAP11 for your installation of Sympa.
Usage: perl Build.PL [OPTIONS]

Configuration:
  -h, --help               display this help and exit

Installation directories:
  --sympa-moduledir DIR    base directory for Sympa perl modules [default: $d_sympa_moduledir]
  --with-unitsdir DIR      directory for systemd unit files [default: $d_build_with_unitsdir]
END
exit;
}

print "Preparing SympaSOAP11 installation...\n";


# take care of these filetypes
$build->add_build_element('other');
$build->add_build_element('wsdl');

$build->notes( 'sympa-moduledir' => Sympa::Constants->MODULEDIR );
$build->notes( 'sympa-modulesubdir' => Sympa::Constants->MODULEDIR . $my_sympa_modulesubdir );
$build->notes( 'sympa-execcgidir' => Sympa::Constants->EXECCGIDIR );
$build->notes( 'sympa-piddir' => Sympa::Constants->PIDDIR );
$build->notes( 'sympa-user' => Sympa::Constants->USER );
$build->notes( 'sympa-group' => Sympa::Constants->GROUP );
$build->notes( 'sympa-sysconfdir' => Sympa::Constants->SYSCONFDIR );
$build->notes( 'unitsdir' => $system_unitsdir );
$build->notes( 'perl' => $ENV{_} );

# set installation path
$build->install_path(lib  => $build->notes('sympa-modulesubdir'));
#$build->install_path(bin => Sympa::Constants->EXECCGIDIR);
$build->install_path(script => Sympa::Constants->EXECCGIDIR);
$build->install_path(conf => Sympa::Constants->SYSCONFDIR);
#$build->install_path(contrib => Sympa::Constants->SYSCONFDIR . '/soap11-contrib');
$build->install_path(contrib => './soap11-contrib');
#$build->install_path(libdoc => '/dev/null');
#$build->install_path(bindoc => '/dev/null');

$build->create_build_script();

print "Build script created with:";
print "\nPerl:         " . $ENV{_};
print "\nModules:      " . Sympa::Constants->MODULEDIR;
print "\nServer:       " . Sympa::Constants->EXECCGIDIR;
print "\nConfig:       " . Sympa::Constants->SYSCONFDIR;
print "\nUser:         " . Sympa::Constants->USER;
print "\nGroup:        " . Sympa::Constants->GROUP;
print "\nPID:          " . Sympa::Constants->PIDDIR;
print "\nSytemd Units: " . $system_unitsdir;
print "\n\n";

print <<END;

Run 'Build' or 'Build build' to prepare for installation.
Run 'Build test' to check if SympaSOAP11 is ready to run.
Run 'Build install' to install. You may provide --destdir DIR to install below this DIR instead of /.

END
