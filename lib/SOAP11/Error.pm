=head1 NAME

Sympa::WWW::SOAP11::Error

=head1 SYNOPSIS

Alternative SOAP Interface for Sympa

=head1 AUTHOR

Immo Goltz, C<< <Immo Goltz <immo.goltz at nothrix.org>> >>

=head1 LICENSE AND COPYRIGHT

Copyright (C) 2014,2015 Goethe-Institut e.V. - Immo Goltz

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

=cut

package Sympa::WWW::SOAP11::Error;

use strict;
use warnings;

#use Sympa::WWW::SOAP11::Config qw( %Config );

# XML modules
use XML::Compile::Util;


use constant ERROR_NS   => 'http://listen.goethe.de/schemas/sympa/error';

################################################################################
# definition of errors
#
################################################################################


# TODO check if detail is needed
#  +{ PolicyException =>
#       { faultcode   => pack_type(SOAP11ENV, 'Server')
#       , faultstring => 'policy exception'
#       , detail      => 
#          { messageId => $error_code
#          , text      => $error_message
#          , variables => [ $error_code, $error_message, ]
#          }
#       }
#   };


################################################################################
# forbidden
# 	return Sympa related forbidden as soap fault
#
################################################################################
sub unauthorized() {
    +{ Fault =>
        { faultcode   => pack_type(ERROR_NS, 'Client.SympaAuthentication')
        , faultstring => "Authentication failed"
        }
     , _RETURN_CODE => 500	# HTTP_INTERNAL_SERVER_ERROR
     , _RETURN_TEXT => 'Internal Server Error'
     };
}

################################################################################
# forbidden
# 	return Sympa related forbidden as soap fault
#
################################################################################
sub forbidden() {
    +{ Fault =>
        { faultcode   => pack_type(Sympa::WWW::SOAP11::Error::ERROR_NS, 'Client.SympaAuthorization')
        , faultstring => "Not authorized"
        }
     , _RETURN_CODE => 500	# HTTP_INTERNAL_SERVER_ERROR
     , _RETURN_TEXT => 'Internal Server Error'
     };
}

################################################################################
# error
# 	return Sympa related error as soap fault
#
################################################################################
sub error($) {
	my $msg = shift;
    +{ Fault =>
        { faultcode   => pack_type(Sympa::WWW::SOAP11::Error::ERROR_NS, 'Client.SympaProcessing')
        , faultstring => $msg
        }
     , _RETURN_CODE => 500	# HTTP_INTERNAL_SERVER_ERROR
     , _RETURN_TEXT => 'Internal Server Error'
     };
}

################################################################################
# failure
# 	return Sympa related fault as soap fault
#
################################################################################
sub failure($) {
	my $msg = shift;
    +{ Fault =>
        { faultcode   => pack_type(Sympa::WWW::SOAP11::Error::ERROR_NS, 'Server.SympaProcessing')
        , faultstring => $msg
        }
     , _RETURN_CODE => 500	# HTTP_INTERNAL_SERVER_ERROR
     , _RETURN_TEXT => 'Internal Server Error'
     };
}

1;
