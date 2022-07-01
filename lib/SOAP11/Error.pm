# SOAP 1.1 / WSSE compliant interface for
# Sympa - SYsteme de Multi-Postage Automatique
#
# Copyright 2013-2022 Goethe-Institut e.V.
# Immo Goltz <immo.goltz@goethe.de>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

package Sympa::WWW::SOAP11::Error;

use strict;
use warnings;

# XML modules
use XML::Compile::Util;


use constant ERROR_NS   => 'http://listen.goethe.de/schemas/sympa/error';

#
# definition of errors
#

#
# unauthorized
#     return Sympa related unauthorized as soap fault
#
sub unauthorized() {
    +{ Fault =>
        { faultcode   => pack_type(ERROR_NS, 'Client.SympaAuthentication')
        , faultstring => "Authentication failed"
        }
     , _RETURN_CODE => 500    # HTTP_INTERNAL_SERVER_ERROR
     , _RETURN_TEXT => 'Internal Server Error'
     };
}

#
# forbidden
#     return Sympa related forbidden as soap fault
#
sub forbidden() {
    +{ Fault =>
        { faultcode   => pack_type(Sympa::WWW::SOAP11::Error::ERROR_NS, 'Client.SympaAuthorization')
        , faultstring => "Not authorized"
        }
     , _RETURN_CODE => 500    # HTTP_INTERNAL_SERVER_ERROR
     , _RETURN_TEXT => 'Internal Server Error'
     };
}

#
# error
#     return Sympa related error as soap fault
#
sub error($) {
    my $msg = shift;
    +{ Fault =>
        { faultcode   => pack_type(Sympa::WWW::SOAP11::Error::ERROR_NS, 'Client.SympaProcessing')
        , faultstring => $msg
        }
     , _RETURN_CODE => 500    # HTTP_INTERNAL_SERVER_ERROR
     , _RETURN_TEXT => 'Internal Server Error'
     };
}

#
# failure
#     return Sympa related fault as soap fault
#
sub failure($) {
    my $msg = shift;
    +{ Fault =>
        { faultcode   => pack_type(Sympa::WWW::SOAP11::Error::ERROR_NS, 'Server.SympaProcessing')
        , faultstring => $msg
        }
     , _RETURN_CODE => 500    # HTTP_INTERNAL_SERVER_ERROR
     , _RETURN_TEXT => 'Internal Server Error'
     };
}

1;
