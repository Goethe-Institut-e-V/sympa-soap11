# SOAP 1.1 / WSSE compliant interface for
# Sympa - SYsteme de Multi-Postage Automatique
#
# Copyright 2013-2019 Goethe-Institut e.V.
# Immo Goltz <immo.goltz.extern@goethe.de>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

package Sympa::WWW::SOAP11;

use strict;
use warnings;
use Encode qw();

#use Sympa;
use Conf;
#use Sympa::Constants;
use Sympa::List;
use Sympa::Log;
use Sympa::Scenario;
#use Sympa::Spindle::ProcessRequest;
#use Sympa::Template;
#use Sympa::Tools::Text;
#use Sympa::User;
use Sympa::WWW::Auth;
#use Sympa::WWW::Session;

# XML::Compile modules
#use XML::Compile::Util;

# debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;

# SOAP11
my $VERSION = '0.5.0'; # Module::Build reads this line
use constant VERSION => '0.5.0';
use Sympa::WWW::SOAP11::Error;


# get logger instance
my $log = Sympa::Log->instance;


# Forward declarations allow prototype checking
sub checkAuth($);

sub getVersion($$);
sub getLists($$);




# authenticate user
sub checkAuth($) {
	my $in = shift;
  
  	#TODO: really needed?
	# Set an env var to find out if in a SOAP context
	# used in Auth.pm only
	$ENV{'SYMPA_SOAP'} = 1;

	# TODO: if wsse ... draus machen
	# user/credentials from wsse
	my $email = $in->{wsse_Security}{wsse_UsernameToken}{wsse_Username}{_} || undef;
	$log->syslog('debug', "email: %s", $email);
	# return unauthenticated if wsse not defined
	unless (defined $email) {
   		$log->syslog('notice', "login authentication failed, no user in wsse" );
		return 0;
	}

	# TODO:
	# Paßwort als Text übertregn ok
	# wie hie rhandeln wenn als digest?
	# TODO: das muß raus hier!!!
	$log->syslog('debug', "pass: %s", $in->{wsse_Security}{wsse_UsernameToken}{wsse_Password}{_});

	my $user = Sympa::WWW::Auth::check_auth(
					$ENV{'SYMPA_ROBOT'},
					$email,
					$in->{wsse_Security}{wsse_UsernameToken}{wsse_Password}{_} || ''
				);
	# return unauthenticated
	unless ($user) {
   		$log->syslog('notice', "login authentication failed for %s", $email);
		return 0;
	}

	$ENV{'USER_EMAIL'} = $email;
   	$log->syslog('debug', "login authentication OK");
	#$log->syslog('debug', Dumper \%ENV);
	
	# return authenticated
	return 1;
}




#
# getVersion
# 	return Sympa and SOAP11 version 
#
sub getVersion($$) {
	my ($server, $in) = @_;
	
	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# return forbidden if not authorized 
	unless ( Sympa::is_listmaster($ENV{'SYMPA_ROBOT'}, $ENV{'USER_EMAIL'}) ) {
		$log->syslog('info', 'not allowed for %s', $ENV{'USER_EMAIL'});
		return Sympa::WWW::SOAP11::Error::forbidden()
	}

	#$log->syslog('debug2', 'Sympa version: %s', Sympa::Constants::VERSION);
	#$log->syslog('debug2', 'SOAP11 version: %s', Sympa::WWW::SOAP11::VERSION);

	# return
	return {
		sympa => Sympa::Constants::VERSION,
		soap => Sympa::WWW::SOAP11::VERSION
	};	
}




#
# getRobots
# 	return all robots of this Sympa instance
#
sub getRobots($$) {
	my ($server, $in) = @_;
	
	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# return forbidden if not authorized (global listmaster)
	unless ( Sympa::is_listmaster('*', $ENV{'USER_EMAIL'}) ) {
		$log->syslog('info', 'not allowed for %s', $ENV{'USER_EMAIL'});
		return Sympa::WWW::SOAP11::Error::forbidden()
	}

	my @robots = Sympa::List::get_robots();
	return {
		robot => \@robots
	};	
}




#
# getListTemplates
#	returns the list templates
#
sub getListTemplates($$) {
	my ($server, $in) = @_;
	
	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# return forbidden if not authorized 
	unless ( Sympa::is_listmaster($ENV{'SYMPA_ROBOT'}, $ENV{'USER_EMAIL'}) ) {
		$log->syslog('info', 'not allowed for %s', $ENV{'USER_EMAIL'});
		return Sympa::WWW::SOAP11::Error::forbidden()
	}

	my @result;  

	# the object with all informations about the templates
	my $tpl = Sympa::WWW::Tools::get_list_list_tpl($ENV{'SYMPA_ROBOT'});

	# helper for parsing the tt2 comment
	my $parser = Sympa::Template->new($ENV{'SYMPA_ROBOT'});

	foreach my $template (keys %{$tpl}) {
		my $parse_result;
		# parse tt2
		unless ($parser->parse({}, [ $tpl->{$template}->{'html_content'} ], \$parse_result)) {
			my $error = $parser->{last_error};
			$error = $error->as_string if ref $error;
			Sympa::send_notify_to_listmaster($ENV{'SYMPA_ROBOT'}, 'web_tt2_error', [$error]);
			$log->syslog('info', 'Error parsing template');
		}
		# remove html
		$parse_result =~ s/\n//g;
		$parse_result =~ s/<\/?ul>//g;
		$parse_result =~ s/<li>//g;
		$parse_result =~ s/<\/li>/; /g;
		$parse_result =~ s/; $//g;
		push @result,  {
			'name' => $template,
			'title' => $tpl->{$template}->{'title'},
			#'comment' => $tpl->{$template}->{'html_content'},
			'comment' => $parse_result,
			};
	}

	$log->syslog('debug3', 'getListTemplates: %s', Dumper \$tpl);

	# return
	return { templates => \@result };
}




#
# getLists
#	returns the names of the lists of this robot
#
sub getLists($$) {
	my ($server, $in) = @_;
	
	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{getListsRequest}{list} || '';
	my $topic = $in->{getListsRequest}{topic} || '';
	#my $subtopic = $in->{getListsRequest}{subtopic} || '';  
	#$log->syslog('info', 'list-pattern: %s, topics: %s/%s; user: %s', $listname, $topic, $subtopic, $ENV{'USER_EMAIL'});
	$log->syslog('info', 'list-pattern: %s, topic: %s; user: %s', $listname, $topic, $ENV{'USER_EMAIL'});
	
	my @result;

	my $all_lists = Sympa::List::get_lists($ENV{'SYMPA_ROBOT'}, 'filter' => [ '%name%' => $listname, topics => $topic ]);
	$log->syslog('debug3', 'getLists %s', Dumper $all_lists);
	
	foreach my $list ( @$all_lists ) {

		my $listname = $list->{'name'};
		$log->syslog('debug3', 'Listname: %s', $listname);
		
		# as /^(send|visibility)$/ fails for pending/closed lists
        my $result = Sympa::Scenario->new($list, 'visibility')->authz('md5', {'sender' => $ENV{'USER_EMAIL'}});
		my $action = $result->{'action'} if (ref($result) eq 'HASH');
		$log->syslog('debug3', 'Action: %s', $action);
		# we check if owner and display all lists 
		next unless ($action eq 'do_it');
		# || $list->am_i('owner', $ENV{'USER_EMAIL'}) );
		
		# no topic ; List all lists
		#if (!$topic) {
		#	push @result, $list->{'name'};
		#	
		#}
		#elsif ($list->{'admin'}{'topics'}) {
		#	foreach my $list_topic (@{$list->{'admin'}{'topics'}}) {
		#		my @tree = split '/', $list_topic;

		#		next if (($topic) && ($tree[0] ne $topic));
		#		next if (($subtopic) && ($tree[1] ne $subtopic));
		#		
		#		push @result, $list->{'name'};
		#	}
		#}
		#elsif ($topic eq 'topicsless') {
		#	push @result, $list->{'name'};
		#}
		
		# above tests not needed, get_lists is is already filtered on topic
		# subtopic considered not so important
		# also removed from wsdl
		push @result, $list->{'name'};

	}
	
	# return 
	$log->syslog('debug2', 'RESULT: %s', Dumper (list => \@result));
	return { list => \@result };
}




#
# getLists
# 	"lists" in sympasoap
#	returns the lists of this robot
#
sub getList($$) {
	my ($server, $in) = @_;
	
	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{getListRequest}{list} || '';
	$log->syslog('info', 'list: %s; user: %s', $listname, $ENV{'USER_EMAIL'});
	
	my @result;

	my $all_lists = Sympa::List::get_lists($ENV{'SYMPA_ROBOT'}, 'filter' => [ name => $listname ]);
	$log->syslog('debug3', 'getList %s', Dumper $all_lists);
	
	foreach my $list ( @$all_lists ) {

		my $listname = $list->{'name'};

		$log->syslog('debug3', 'Listname: %s', $listname);
		
		my $result_item = {};
		# as /^(send|visibility)$/ fails for pending/closed lists
        my $result = Sympa::Scenario->new($list, 'visibility')->authz('md5', {'sender' => $ENV{'USER_EMAIL'}});
		my $action;
		$action = $result->{'action'} if (ref($result) eq 'HASH');
		$log->syslog('debug3', 'Action: %s', $action);
		# we check if owner and display all lists 
		next unless ($action eq 'do_it');
		# || $list->am_i('owner', $ENV{'USER_EMAIL'}) );
		
		# building result packet
		$result_item->{'listAddress'} = Sympa::get_address($list);
		$result_item->{'subject'} = $list->{'admin'}{'subject'};
		$result_item->{'subject'} =~ s/;/,/g; # TODO: why this?
        $result_item->{'homepage'} = Sympa::get_url($list, 'info');
		
		$result_item->{'name'} = $list->{'name'};
		$result_item->{'domain'} = $list->{'domain'};
		$result_item->{'visibility'} = $list->{'admin'}{'visibility'}{'name'};
		$result_item->{'status'} = $list->{'admin'}{'status'};
		$result_item->{'lang'} = $list->{'admin'}{'lang'};
		$result_item->{'topic'} = $list->{'admin'}{'topics'};
		$result_item->{'subscriberCount'} = $list->get_total();
		# from stats file, e.g. 1 5 619 3095 5 0 1400593718
		# number of messages sent, used to generate X-sequence headers
		#	aka number of messages sent to the list $list->get_msg_count ()
		# number of messages X number of recipients;
		# number of bytes X number of messages;
		# number of bytes X number of messages X number of recipients;
		# The last three fields total, last_sync and last_sync_admin_user were deprecated.
		my @stats = $list->get_stats();
		$result_item->{'msgCount'} = $stats[0];
		$result_item->{'msgCountXRecipients'} = $stats[1];
		# in List.pm created as $today = int(time / 86400);
		$result_item->{'latestDistributionDate'} = localtime( 86400 * $list->get_latest_distribution_date() );

		# description is in /info file
		if (-r $list->{'dir'}.'/info') {
			my $file_path = $list->{'dir'}.'/info';
			unless (open FILE, "<", $file_path) {
				$log->syslog('err', 'getLists: failed to open file %s: %s', $file_path,$!);
			}
			while (<FILE>) {
				Encode::from_to($_, $Conf::Conf{'filesystem_encoding'}, 'utf8');
				$result_item->{'description'} .= $_;
			}
			close FILE;
		}

		push @result, $result_item;
		$log->syslog('debug2', 'LISTE FULL: %s', Dumper \$list );

	}
	
	# return 
	$log->syslog('debug2', 'RESULT: %s', Dumper (list => \@result));
	return { list => \@result };
}




#
# createList
# 	"createList" in sympasoap
#   "do_create_list" in wwwsympa.fcgi
#	creates a list
#
sub createList($$) {
	my ($server, $in) = @_;

	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname  = $in->{createListRequest}{name};
	my $subject = $in->{createListRequest}{subject};
	my $list_tpl =  $in->{createListRequest}{template};
	my $description =  $in->{createListRequest}{description};
	my $topics =  $in->{createListRequest}{topics};
	# topics is mandatory in sympa
	$topics = 'other' unless $topics;
	#FIXME: lang is not set in Spindle
	my $lang =  $in->{createListRequest}{lang};
	# FIXME: zumindest mit perlbrew installtion ist es mit decode falsch! Ohne ist OK!
	# FIXME:    siehe auch addSubscriber, dort ist nun (mit perlbrew?) decode nötig! Ohne war es wieder defekt.
	# needs decode to be correct in saved list description file, in web gui and sympa-soap11 (getList)
	# $subject = &Encode::decode('UTF8', $subject);

    my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};
    #my $remote_application_name = 'soap11';

	$log->syslog(
        'info',
        '(listname=%s, subject=%s, template=%s, description=%s, topics=%s, lang=%s, sender=%s, robot=%s)',
        $listname, $subject, $list_tpl, $description, $topics, $lang, $sender, $robot
    );

    my $user = Sympa::User::get_global_user($sender)
        if Sympa::User::is_global_user($sender);

    my $spindle = Sympa::Spindle::ProcessRequest->new(
        context    => $robot,
        action     => 'create_list',
        listname   => $listname,
        parameters => {
            owner => [
                {   email => $sender,
                    gecos => ($user ? $user->{gecos} : undef),
                }
            ],
            subject        => $subject,
            creation_email => $sender,
            template       => $list_tpl,
            topics         => $topics,
            description    => $description,
			lang           => $lang,
        },
        sender    => $sender,
        md5_check => 1,

        scenario_context => {
            'sender'                  => $sender,
            'candidate_listname'      => $listname,
            'candidate_subject'       => $subject,
            'candidate_template'      => $list_tpl,
            'candidate_info'          => $description,
            'candidate_topics'        => $topics,
            'remote_host'             => $ENV{'REMOTE_HOST'},
            'remote_addr'             => $ENV{'REMOTE_ADDR'},
            #'remote_application_name' => 'soap11'
        }
    );

    unless ($spindle and $spindle->spin) {
		$log->syslog('err', 'spindle cant spin');
		return Sympa::WWW::SOAP11::Error::failure('Internal error');
    }

    foreach my $report (@{$spindle->{stash} || []}) {
        my $reason_string = get_reason_string($report, $robot);
        if ($report->[1] eq 'auth') {
			$log->syslog('err', 'report:auth, reason: %s', $reason_string);
			return Sympa::WWW::SOAP11::Error::error($reason_string);
        } elsif ($report->[1] eq 'intern') {
			$log->syslog('err', 'report:intern, reason: %s', $reason_string);
			return Sympa::WWW::SOAP11::Error::error($reason_string);
        } elsif ($report->[1] eq 'notice') {
			$log->syslog('info', 'report:notice, reason: %s', $reason_string);
			# seen $reason_string: Aliases have been installed.
		    #return { status => 'OK' };
        } elsif ($report->[1] eq 'user') {
			$log->syslog('err', 'report:user, reason: %s', $reason_string);
			return Sympa::WWW::SOAP11::Error::error($reason_string);
        }
    }

	$log->syslog('info', 'no report, OK');

	# workaround: spindle does not set lang
	my $list = Sympa::List->new($listname, $robot);
	$list->{'admin'}{'lang'} = $lang;
	delete $list->{'admin'}{'defaults'}{'lang'};
	$list->save_config($sender);

    return { status => 'OK' };
}


#FIXME: $reason_string ist jetzt (2.4.48) unlesbar (encoding mismatch?!)
# oder weil Liste auf ru-RU steht?
#delSubscriber lieferte kyrillischen text <sch:status>Undef. Почтовый адрес  не был найден в рассылке .</sch:status>
#addSubscriber <sch:status>Undef. ÐÐ¾Ð»ÑÐ·Ð¾Ð²Ð°ÑÐµÐ»Ñ '999-1@localhost.localdomain' ÑÐ¶Ðµ ÑÐ²Ð»ÑÐµÑÑÑ Ð¿Ð¾Ð´Ð¿Ð¸ÑÑÐ¸ÐºÐ¾Ð¼ ÑÐ°ÑÑÑÐ»ÐºÐ¸ 'liste-999'.</sch:status>
#createList <faultstring>ÑÐ°ÑÑÑÐ»ÐºÐ° Ñ Ð¸Ð¼ÐµÐ½ÐµÐ¼ 'liste-999' ÑÐ¶Ðµ ÑÑÑÐµÑÑÐ²ÑÐµÑ</faultstring>
# aber direkt nach start kommt <faultstring>'liste-999' list already exists</faultstring>,
# debug2 ausgabe auf konsole ist aber ok, in kyrillisch
#
# helper function to translate with tt2 files
#   used (at least) in createList
#
sub get_reason_string {
    my $report = shift;
    my $robot  = shift;

    my $data = {
        report_type  => $report->[1],
        report_entry => $report->[2],
        report_param => {
            action => $report->[0]->{action},
            %{$report->[3] || {}},
        },
    };
    my $string;

    my $template = Sympa::Template->new($robot, subdir => 'mail_tt2');
    unless ($template->parse($data, 'report.tt2', \$string)) {
        my $error = $template->{last_error};
        $error = $error->as_string if ref $error;
        Sympa::send_notify_to_listmaster($robot, 'web_tt2_error', [$error]);
        $log->syslog('info', 'Error parsing');
        return '';
    }
	# FIXME: debugging
    $log->syslog('debug2', 'Report Elements: %s, %s, %s', $report->[1], $report->[2], $report->[0]->{action});
	$log->syslog('debug2', 'Reason String: %s', $string);
	$string = &Encode::decode('UTF8', $string);
	$log->syslog('debug2', 'Reason String: %s', $string);

    return $string;
}




#
# getSubscribers
# "review" in sympasoap
#	returns all subscribers of a list
#
sub getSubscribers($$) {
	my ($server, $in) = @_;

	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{getSubscribersRequest}{name} || '';

    my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};

    $log->syslog('debug', '(%s, %s)', $listname, $robot);

	# return error if unknown list
    my $list = Sympa::List->new($listname, $robot);
    unless ($list) {
        $log->syslog('info',
            'Review %s from %s refused, list unknown to robot %s',
            $listname, $sender, $robot);
		return Sympa::WWW::SOAP11::Error::error("No such list");
    }

	# Part of the authorization code
	my $user = Sympa::User::get_global_user($sender);

    my $result = Sympa::Scenario->new($list, 'review')->authz(
        'md5',
        {   'sender'                  => $sender,
        }
    );
    my $action = $result->{'action'} if (ref($result) eq 'HASH');

	# return failure
	return Sympa::WWW::SOAP11::Error::failure('No action available')
	unless (defined $action);

	# return forbidden if not authorized
	if ($action =~ /reject/i) {
        my $reason_string = get_reason_string(
            [{action => 'review'}, 'auth', $result->{'reason'}], $robot);
        $log->syslog('info', 'Review %s from %s refused (not allowed)',
            $listname, $sender);
		# TODO: reason_string mit ausgeben?
		return  Sympa::WWW::SOAP11::Error::forbidden();
	}
	
	if ($action =~ /do_it/i) {
		my @result;
        my $is_owner = $list->is_admin('owner', $sender)
            || Sympa::is_listmaster($list, $sender);

        # Members list synchronization if include is in use
        if ($list->has_include_data_sources()) {
            unless (defined $list->on_the_fly_sync_include(use_ttl => 1)) {
                $log->syslog('notice', 'Unable to synchronize list %s', $list);
            }
        }

        unless ($user = $list->get_first_list_member({'sortby' => 'email'})) {
            $log->syslog('err', 'No subscribers in list "%s"', $list->{'name'});
			return { subscribers => \@result };
		}
		$log->syslog('debug2', 'pre-result: %s', Dumper \@result);

		do {
            # Owners bypass the visibility option
            unless (($user->{'visibility'} eq 'conceal')
                and (!$is_owner)) {

                # Lower case email address
                $user->{'email'} =~ y/A-Z/a-z/;
				push @result, $user;
			}
        } while ($user = $list->get_next_list_member());

        $log->syslog('info', 'Review %s from %s accepted', $listname, $sender);
		$log->syslog('debug2', 'result: %s', Dumper \@result);
		return { subscribers => \@result };
	}

	# return failure
    $log->syslog('info',
        'Review %s from %s aborted, unknown requested action %s in scenario',
        $listname, $sender, $action);
	return Sympa::WWW::SOAP11::Error::failure("Unknown requested action $action in scenario");
}




#
# addSubscriber
#	"add" in sympasoap
#	adds a subscriber to a list (without double opt in!)
#
sub addSubscriber($$) {
	my ($server, $in) = @_;
	
	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{addSubscriberRequest}{name} || '';
	my $quiet = $in->{addSubscriberRequest}{quiet} || 0;

    my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};

	# return error if unknown list
    my $list = Sympa::List->new($listname, $robot);
    unless ($list) {
        $log->syslog('info',
            'Add to list %s by %s refused, list unknown to robot %s',
            $listname, $sender, $robot);
		return Sympa::WWW::SOAP11::Error::error("No such list");
    }

	# Note: there is a spindle action "import" which imports a list of addresses, see wwwsympa.fcgi, sub do_import {
	my @result;
	my $total_sub = 0;
	my $ok_sub = 0;
	# data for each Subscriber
	foreach my $subscriber ( @{$in->{addSubscriberRequest}{subscriber}} ) {
		$total_sub++;
		my $email = $subscriber->{email} || '';
		my $gecos = $subscriber->{gecos} || '';
		# encode UTF8 submitted data
		#$gecos = &Encode::encode('UTF8', $gecos);
		# without encode hÃ¼Ã¶Ã¤  | hÃ¼Ã¶Ã¤
		# with encode hÃÂ¼ÃÂ¶ÃÂ¤   | 
		#$gecos = &Encode::decode('UTF8', $gecos);
		# with decode hüöä
		# TODO: folgende zeile noch nötig???
		$gecos = &Encode::decode('UTF8', $gecos);
		#FIXME: nein, das wohl alles ok so jetzt
		#FIXME: mh, 7.10.19 es braucht doch die zeile jetzt. Warum? Perl mit perlbrew vs. rpm perl????

		# TODO: from old version, stil valid??
		# WARNING: without little patch to sympa mail.pm the welcome mail will be horrible broken
		# if subject/info of list ist UTF8
		# TODO check if only if kyrillic text

		my $status = '';

		my $spindle = Sympa::Spindle::ProcessRequest->new(
			context          => $list,
			action           => 'add',
			sender           => $sender,
			email            => $email,
			gecos            => $gecos,
			quiet            => $quiet,
			md5_check        => 1,
			scenario_context => {
				sender                  => $sender,
				email                   => $email,
				#remote_host             => $ENV{'REMOTE_HOST'},
				#remote_addr             => $ENV{'REMOTE_ADDR'},
				#remote_application_name => $ENV{'remote_application_name'}
			}
		);
		unless ($spindle and $spindle->spin) {
			$log->syslog('err',
				'Add %s to list %s by %s in robot %s failed. Internal error',
				$email, $listname, $sender, $robot);			
			push @result, { email => $email, status => 'Internal error' };
			next;
		}

		foreach my $report (@{$spindle->{stash} || []}) {
			my $reason_string = get_reason_string($report, $robot);
			if ($report->[1] eq 'auth') {
				push @result, { email => $email, status => 'Not allowed. ' . $reason_string };
				next;
			} elsif ($report->[1] eq 'intern') {
				push @result, { email => $email, status => 'Internal error' };
				next;
			} elsif ($report->[1] eq 'notice') {
				push @result, { email => $email, status => 'OK' };
				$ok_sub++;
				next;
			} elsif ($report->[1] eq 'user') {
				push @result, { email => $email, status => 'Undef. ' . $reason_string };
				next;
			}
		}

	}

	my $fail_sub = $total_sub - $ok_sub;

	$log->syslog('debug2', 'addSubscriber result: %s', Dumper \@result );
	return { result => { subscribed => $ok_sub, failed => $fail_sub }, subscriber => \@result };
}




#
# delSubscriber
#	"del" in sympasoap
#	removees a subscriber from a list (without double opt out!)
#
sub delSubscriber($$) {
	my ($server, $in) = @_;

	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{delSubscriberRequest}{name} || '';
	my $quiet = $in->{delSubscriberRequest}{quiet} || 0;

    my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};

	# return error if unknown list
    my $list = Sympa::List->new($listname, $robot);
    unless ($list) {
        $log->syslog('info',
            'Add to list %s by %s refused, list unknown to robot %s',
            $listname, $sender, $robot);
		return Sympa::WWW::SOAP11::Error::error("No such list");
    }

	my @result;
	my $total_sub = 0;
	my $ok_sub = 0;
	# data for each Subscriber
	foreach my $subscriber ( @{$in->{delSubscriberRequest}{subscriber}} ) {
		$total_sub++;
		my $email = $subscriber->{email} || '';

	 	my $status = '';

		my $spindle = Sympa::Spindle::ProcessRequest->new(
			context          => $list,
			action           => 'del',
			sender           => $sender,
			email            => $email,
			quiet            => $quiet,
			md5_check        => 1,
			scenario_context => {
				sender                  => $sender,
				email                   => $email,
				#remote_host             => $ENV{'REMOTE_HOST'},
				#remote_addr             => $ENV{'REMOTE_ADDR'},
				#remote_application_name => $ENV{'remote_application_name'}
			}
		);
		unless ($spindle and $spindle->spin) {
			$log->syslog('err',
				'Remove %s from list %s by %s in robot %s failed. Internal error',
				$email, $listname, $sender, $robot);			
			push @result, { email => $email, status => 'Internal error' };
			next;
		}

		foreach my $report (@{$spindle->{stash} || []}) {
			my $reason_string = get_reason_string($report, $robot);
			if ($report->[1] eq 'auth') {
				push @result, { email => $email, status => 'Not allowed. ' . $reason_string };
				next;
			} elsif ($report->[1] eq 'intern') {
				push @result, { email => $email, status => 'Internal error' };
				next;
			} elsif ($report->[1] eq 'notice') {
				push @result, { email => $email, status => 'OK' };
				$ok_sub++;
				next;
			} elsif ($report->[1] eq 'user') {
				push @result, { email => $email, status => 'Undef. ' . $reason_string };
				next;
			}
		}

	}

	my $fail_sub = $total_sub - $ok_sub;

	$log->syslog('debug2', 'delSubscriber result: %s', Dumper \@result );
	return { result => { unsubscribed => $ok_sub, failed => $fail_sub }, subscriber => \@result };
}




#
# getSubscriptions
# 	'which' in sympasoap
#	returns the lists a email (user) is subscribed to
#   Note: there is a request spindle "which" but informs the requester via mail
#
sub getSubscriptions($$) {
	my ($server, $in) = @_;
	
	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);
	
	# get parameters
	my $email = $in->{getSubscriptionsRequest}{email} || '';

    my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};
	
	my @result;
	my %listnames;

    foreach my $role ('member', 'owner', 'editor') {
        foreach my $list (Sympa::List::get_which($email, $robot, $role)) {
            my $name = $list->{'name'};
            $listnames{$name} = $list;
        }
    }
	
	foreach my $name (keys %listnames) {
		my $list = $listnames{$name};
		my $list_address;
		my $result_item;
		
        my $result = Sympa::Scenario->new($list, 'visibility')->authz(
            'md5',
            {   'sender'                  => $sender,
                #'remote_application_name' => $ENV{'remote_application_name'}
            }
        );
		my $action;
		$action = $result->{'action'} if (ref($result) eq 'HASH');
		next unless ($action =~ /do_it/i);
		
		$result_item->{'name'} = $list->{'name'};
		$result_item->{'address'} = Sympa::get_address($list);
		$result_item->{'subject'} = $list->{'admin'}{'subject'};
		$result_item->{'subject'} =~ s/;/,/g;

        # determine status of user
        $result_item->{'owner'} = 0;
        if ($list->is_admin('owner', $email)
            or Sympa::is_listmaster($list, $email)) {
            $result_item->{'owner'} = 1;
        }
        $result_item->{'editor'} = 0;
        if ($list->is_admin('actual_editor', $email)) {
            $result_item->{'editor'} = 1;
        }
        $result_item->{'subscribed'} = 0;
        if ($list->is_list_member($email)) {
            $result_item->{'subscribed'} = 1;
        }

		# determine bounce information of this user for this list
        if ($result_item->{'subscribed'}) {
            if (my $subscriber = $list->get_list_member($email)) {
				$list->parse_list_member_bounce($subscriber);
                $result_item->{'subscriber'} = $subscriber;
				$log->syslog('debug2', 'subscriber: %s', Dumper \$subscriber);
            }
        }

		push @result, $result_item;	
	}

	$log->syslog('debug2', 'getSubscriptions: %s', Dumper \@result);
   	return { list => \@result };
}




#
# getAdmins
# 	get list of admins (owners/editors)
#   get_admins in List.pm
#
sub getAdmins($$) {
	my ($server, $in) = @_;
	
	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{getAdminsRequest}{name} || '';
	my $role = $in->{getAdminsRequest}{role} || '';
    my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};

	# return error if unknown list
    my $list = Sympa::List->new($listname, $robot);
    unless ($list) {
        $log->syslog('info', 'No such list %s@%s', $listname, $robot);
		return Sympa::WWW::SOAP11::Error::error("No such list");
    }

	# return forbidden if not authorized 
	# FIXME: nur role owner allowed? check may_edit; now editors also are able to list
	unless ($list->is_admin($role, $sender) || Sympa::is_listmaster($list, $sender)) {
		$log->syslog('info', 'Review %s of list %s@%s not allowed for %s', $role, $listname, $robot, $sender);
		return Sympa::WWW::SOAP11::Error::forbidden()
	}

	#TODO: do we need distinction to privileged_owner?
	# get owners
	my $result = $list->get_admins($role);

	$log->syslog('debug2', Dumper $result);
	return { admin => $result };
}





#
# addAdmins
# 	add admins to list (owners/editors)
#   see List.pm add_list_admin/_add_list_admin
#
sub addAdmins($$) {
	my ($server, $in) = @_;

	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{addAdminsRequest}{name} || '';
	my $role = $in->{addAdminsRequest}{role} || '';
	my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};

	# return error if unknown list
    my $list = Sympa::List->new($listname, $robot);
    unless ($list) {
        $log->syslog('info', 'No such list %s@%s', $listname, $robot);
		return Sympa::WWW::SOAP11::Error::error("No such list");
    }

	#FIXME: may_edit? nur privileged_owner zB dürfen
	# return forbidden if not authorized 
	unless ($list->is_admin($role, $sender) || Sympa::is_listmaster($list, $sender)) {
		$log->syslog('info', 'Adding %s to list %s@%s not allowed for %s', $role, $listname, $robot, $sender);
		return Sympa::WWW::SOAP11::Error::forbidden()
	}

	my %status = ();
	my @result;
	my $total_add = 0;
	my $ok_add = 0;

	# get parameters for each admin
	# and try to add admin
	foreach my $admin ( @{$in->{addAdminsRequest}{admin}} ) {
		$total_add++;
		my $email = $admin->{email} || '';
		#my $gecos = $admin->{gecos} || '';
		# seit 2.4.48 ?? oder plötzlich? 
		$admin->{gecos} = &Encode::decode('UTF8', $admin->{gecos});
		#my $profile = $admin->{profile} || 'normal';
		#my $reception = $admin->{reception} || 'mail';
		#my $visibility = $admin->{visibility} || 'noconceal';

		# ommit if already has role 
		if ( $list->is_admin($role, $email) ) {
			$log->syslog('info', '%s already %s of the list %s@%s',$email, $role, $listname, $ENV{'SYMPA_ROBOT'});
			$status{$email} = "Already is $role";
			next;
		}

		#FIXME: if supplied users are identical status gets overwritten
		
		# Note: add_list_admin($role, $admin) calls _add_list_admin($role, $admin) for each $admin then
		#       does $list->_cache_publish_expiry('admin_user');
		#       After this we have to create new Sympa::List->new($listname, $robot); instance
		if ($list->_add_list_admin($role, $admin)) {
			$ok_add++;
			$log->syslog('info', 'added %s as %s of the list %s@%s', $email, $role, $listname, $robot);
			# Notify the new list owner/editor
			#FIXME: auch wenn das fehlschlägt tief in sympa gibt es ok zurück, geht es mit eval?
			unless ( Sympa::send_notify_to_user(
				$list,
				'added_as_listadmin',
				$email,
				{   admin_type => $role,
					delegator  => $sender
				}
			) ) {
				$log->syslog('err',"Unable to notify new $role $email");
				$status{$email} = "Unable to notify new $role";
			} else {
				$log->syslog('info',"$email notified");
				$status{$email} = "OK";
			}
		} else {
			$log->syslog('err', 'Failed to add %s as %s of the list %s@%s', $email, $role, $listname, $robot);
			$status{$email} = "Failed.";
		}

	}
	# invalidate cache and reload list
    $list->_cache_publish_expiry('admin_user');
	$list = Sympa::List->new($listname, $robot);

	# get fresh admin objects 
	foreach my $admin ( @{$in->{addAdminsRequest}{admin}} ) {
		my $email = $admin->{email} || '';
		push @result, { email => $email,
						status => $status{$email},
						admin => $list->get_admins($role, filter => [email => $email])
					}; 
	}

	my $fail_add = $total_add - $ok_add;

	$log->syslog('debug2','addOwner: %s', Dumper \@result);
	return { result => { added => $ok_add, failed => $fail_add }, admin => \@result };
}




#
# delOwner
#	remove admins from list (owners/editors)
#   see List.pm delete_list_admin
#   and wwsympa.fcgi
#
sub delAdmins($$) {
	my ($server, $in) = @_;

	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{delAdminsRequest}{name} || '';
	my $role = $in->{delAdminsRequest}{role} || '';
	my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};

	# return error if unknown list
    my $list = Sympa::List->new($listname, $robot);
    unless ($list) {
        $log->syslog('info', 'No such list %s@%s', $listname, $robot);
		return Sympa::WWW::SOAP11::Error::error("No such list");
    }
	
	## return forbidden if not authorized 
	#unless ($list->is_admin('privileged_owner', $sender)
    #        || Sympa::is_listmaster($list, $sender)) {
	#	$log->syslog('info', 'Removing owners from list %s@%s not allowed for %s', $listname, $robot, $sender);
	#	return Sympa::WWW::SOAP11::Error::forbidden()
	#}
#FIXME: is_admin oder may_edit?
	# return forbidden if not authorized 
	unless ( $list->may_edit($role, $sender) eq 'write' ) {
		$log->syslog('info', 'Remove %s from list %s@%s not allowed for %s', $role, $listname, $robot, $sender);
		return Sympa::WWW::SOAP11::Error::forbidden()
	}

	my %status = ();
	my @result;
	my $total_add = 0;
	my $ok_add = 0;

	# get parameters for each admin
	# and try to del admin
	foreach my $email ( @{$in->{delAdminsRequest}{email}} ) {
		$total_add++;

		# List.pm has no _add_list_admin equivalent for del, so we need to refresh list object after each delete
		# invalidate cache and reload list
#6.2.48		$list->_cache_publish_expiry('admin_user');
#6.2.48		$list = Sympa::List->new($listname, $robot);
		my $status = $list->load($listname, $robot);
		$log->syslog('debug2', Dumper $status);

		# return error if not role
		unless ( $list->is_admin($role, $email) ) {
			$log->syslog('info', '%s not %s of the list %s@%s', $email, $role, $listname, $robot);
			#return Sympa::WWW::SOAP11::Error::error("Is not $role of the list");
			$status{$email} = "Is not $role";
			next;
		}
	
		$log->syslog('debug2', Dumper \@{$list->get_admins($role)});
		$log->syslog('debug2', '### ANZAHL sc %s',  scalar(@{$list->get_admins($role)}));
		$log->syslog('debug2', '### ANZAHL  %s',  $#{$list->get_admins($role)});
		# return error if last owner
		#FIXME: der letzte wird trotzdem gelöscht
		#FIXME: nur für owner
		unless ( scalar(@{$list->get_admins($role)}) > 1 ) {
			$log->syslog('info', '%s last %s of the list %s@%s', $email, $role, $listname, $robot);
			#return Sympa::WWW::SOAP11::Error::error("Is last $role of the list");
			$status{$email} = "Is last $role";
			next;
		}
	
		#TODO: und last privileges User? der kann entfernt werden ohne neuen anzulegen

		# delete user
		unless ( $list->delete_list_admin($role, $email) ) {
			$log->syslog('err', 'Failed to remove %s %s from list %s@%s', $role, $email, $listname, $robot);
			#return Sympa::WWW::SOAP11::Error::error("Failed to remove $role $email from $listname\@$robot");
			$status{$email} = "Failed to delete";
			next;
		}

		$ok_add++;
		$status{$email} = 'OK';
		$log->syslog('info', 'Removed %s %s from list %s@%s', $role, $email, $listname, $robot);
	}

	# invalidate cache and reload list
	#$list->_cache_publish_expiry('admin_user');
#6.2.48	$list = Sympa::List->new($listname, $robot);

	#FIXME: all die add/delete funktionieren nicht richtig. caching? nach restart der Anwendung sieht es ok aus.
	# Don't let a list without a privileged admin
	unless ( scalar(@{$list->get_admins('privileged_owner')}) ) {
		for my $admin ($list->get_admins('owner')) {
			my $email = $admin->{email};
			# invalidate cache and reload list
			#$list->_cache_publish_expiry('admin_user');
#6.2.48			$list = Sympa::List->new($listname, $robot);
			unless ( $list->update_list_admin($email, 'owner', {profile => 'privileged'}) ) {
				$log->syslog('err', 'Failed to promote %s as new privileged owner in list %s@%s', $email, $listname, $robot);
				$status{$email} .= ". Failed to promote " . $email . " as new privileged owner.";
				next;
			}
			$status{$email} .= ". Promoted " . $email . " as new privileged owner.";
			$log->syslog('debug2', 'Promoted %s as new privileged owner.', $email);
		}
	}
#FIXME:
#FIXME: addAdmin -> getAdmins funktioniert
#FIXME: delAdmin -> getAdmins (*x)/ addAdmins (*1) zeigt alte Werte
#FIXME: delAdmins -> x* delAdmins oder 1* addAdmins -> getAdmins korrekt
#FIXME:
#FIXME: oh, mit 6.2.48 geht es plötzlich

	# invalidate cache and reload list
#    $list->_cache_publish_expiry('admin_user');
#    $list->_cache_read_expiry('admin_user');
#	$list = Sympa::List->new($listname, $robot);
 #   $list->_cache_publish_expiry('admin_user');
#	$list = Sympa::List->new($listname, $robot);

	# get fresh admin objects 
	foreach my $email ( @{$in->{delAdminsRequest}{email}} ) {
		#my $email = $admin->{email} || '';
		push @result, { email => $email,
						status => $status{$email},
					}; 
	}

	my $fail_add = $total_add - $ok_add;

	$log->syslog('debug2','addOwner: %s', Dumper \@result);
	return { result => { deleted => $ok_add, failed => $fail_add }, admin => \@result };
}




#
# changeEmail
# Changes a user's email address in Sympa environment
#    "do_move_user" in wwsympa.fcgi
#
sub changeEmail($$) {
	my ($server, $in) = @_;
	
	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $current_email = $in->{changeEmailRequest}{old};
	my $email = $in->{changeEmailRequest}{new};

    my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};

    $current_email = Sympa::Tools::Text::canonic_email($current_email);
    $email = Sympa::Tools::Text::canonic_email($email);

    unless (Sympa::Tools::Text::valid_email($current_email)
        and Sympa::Tools::Text::valid_email($email)) {
		return Sympa::WWW::SOAP11::Error::error("Invalid email address");
    }

    # Prevent changing addresses of others unless user is listmaster.
    unless (Sympa::is_listmaster($robot, $sender)
        or $sender eq $current_email) {
        return Sympa::WWW::SOAP11::Error::forbidden();
    }

    # Do the move_user
    my $spindle = Sympa::Spindle::ProcessRequest->new(
        context          => $robot,
        action           => 'move_user',
        current_email    => $current_email,
        email            => $email,
        sender           => $sender,
        md5_check        => 1,
        scenario_context => {
            sender        => $sender,
            current_email => $current_email,
            email         => $email,
        }
    );
    unless ($spindle and $spindle->spin) {
		$log->syslog('err', 'Failed to change user email address. Internal error');
		return Sympa::WWW::SOAP11::Error::error("Failed to change user email address. Internal error");
    }

	$log->syslog('debug2', 'SPINDLE: %s', Dumper $spindle);

    foreach my $report (@{$spindle->{stash} || []}) {
		$log->syslog('debug2', 'STASH definiert %s', Dumper $report);
		#FIXME:  welche Meldungen/Ergebnissse kommen denn hier?
        if ($report->[1] eq 'notice') {
			$log->syslog('debug2', 'Stash notice');
            #Sympa::WWW::Report::notice_report_web(@{$report}[2, 3],$param->{'action'});
			return Sympa::WWW::SOAP11::Error::error("Failed to change email address. " . @{$report}[1] . ": " . @{$report}[2]);
        } else {
            #Sympa::WWW::Report::reject_report_web(@{$report}[1 .. 3],$param->{action});
			$log->syslog('debug2', 'Stash anderes');
			return Sympa::WWW::SOAP11::Error::error("Failed to change email address. " . @{$report}[1] . ": " . @{$report}[2]);
        }

    }

	my %result;
	#FIXME: unless check not needed, if there is a stash a soap fault is returned above
    unless (@{$spindle->{stash} || []}) {
		$log->syslog('info', 'changed email %s to %s', $current_email, $email);
		$result{ status } = "OK";
    }
	
	$log->syslog('debug2', Dumper \%result);
	return { result => \%result };
}


sub closeList($$) {
	my ($server, $in) = @_;
	$in->{closeListRequest}{mode} = 'close';
	_closeList($server, $in);
}

sub deleteList($$) {
	my ($server, $in) = @_;
	$in->{closeListRequest}{name} = $in->{deleteListRequest}{name};
	$in->{closeListRequest}{mode} = 'purge';
	_closeList($server, $in);
}

#
# closeList
#	closeList in sympasoap
#	close (disable) or
#	purge (completely remove) a list
#
sub _closeList($$) {
	my ($server, $in) = @_;

	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{closeListRequest}{name} || '';
	my $mode = $in->{closeListRequest}{mode} || 'close';
	my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};

	# return error if unknown list
    my $list = Sympa::List->new($listname, $robot);
    unless ($list) {
        $log->syslog('info', 'No such list %s@%s', $listname, $robot);
		return Sympa::WWW::SOAP11::Error::error("No such list");
    }
	
	# return forbidden if not authorized 
	unless ($list->is_admin('owner', $sender)
            || Sympa::is_listmaster($list, $sender)) {
		$log->syslog('info', '%s list %s@%s not allowed for %s', $mode, $listname, $robot, $sender);
		return Sympa::WWW::SOAP11::Error::forbidden()
	}

    my $spindle = Sympa::Spindle::ProcessRequest->new(
        context      => $list->{'domain'},
        action       => 'close_list',
        current_list => $list,
        mode => $mode,
            #(($list->{'admin'}{'status'} eq 'pending') ? 'purge' : 'close'),
        sender           => $sender,
        md5_check        => 1,
        scenario_context => {
            sender                  => $sender,
            #remote_host             => $ENV{'REMOTE_HOST'},
            #remote_addr             => $ENV{'REMOTE_ADDR'},
            #remote_application_name => $ENV{'remote_application_name'}
        }
    );

    unless ($spindle and $spindle->spin) {
        die SOAP::Fault->faultcode('Server')->faultstring('Internal error');
    }

	foreach my $report (@{$spindle->{stash} || []}) {
		my $reason_string = get_reason_string($report, $robot);
		if ($report->[1] eq 'auth') {
			$log->syslog('info', '%s list %s@%s not allowed for %s', $mode, $listname, $robot, $sender);
	        return Sympa::WWW::SOAP11::Error::forbidden();
		} elsif ($report->[1] eq 'intern') {
			$log->syslog('err', 'Failed to %s list %s. Internal error', $mode, $listname);
			return Sympa::WWW::SOAP11::Error::error("Failed to $mode list $listname. Internal error");
		} elsif ($report->[1] eq 'notice') {
			$log->syslog('info', '%s list %s success', $mode, $listname);
			return { status => 'OK' , message => $reason_string };
		} elsif ($report->[1] eq 'user') {
			$log->syslog('err', 'Failed to %s list %s. Undef. %s', $mode, $listname, $reason_string);
			return Sympa::WWW::SOAP11::Error::error("Failed to $mode list $listname. $reason_string");
			next;
		}
	}

	# do we reach this point?
	$log->syslog('info', '%s list %s success', $mode, $listname);
	return { status => 'OK' };
}






# not yet migrated/needed?
################################################################################
# getListDefinition
# 	get attributes of a list
#
################################################################################
sub getListDefinition($$) {
	my ($server, $in) = @_;

	# check auth
	return Sympa::WWW::SOAP11::Error::unauthorized() 
	if not checkAuth($in);

	# get parameters
	my $listname = $in->{getListDefinitionRequest}{name} || '';
	my $sender                  = $ENV{'USER_EMAIL'};
    my $robot                   = $ENV{'SYMPA_ROBOT'};

	# return error if unknown list
    my $list = Sympa::List->new($listname, $robot);
    unless ($list) {
        $log->syslog('info', 'No such list %s@%s', $listname, $robot);
		return Sympa::WWW::SOAP11::Error::error("No such list");
    }
	
	# return forbidden if not authorized 
	# TODO: wer darf das wirklich sehen? listmaster, owner?
	unless ($list->is_admin('privileged_owner', $sender)
            || Sympa::is_listmaster($list, $sender)) {
		$log->syslog('info', 'Inspecting parameters of list %s@%s not allowed for %s', $listname, $robot, $sender);
		return Sympa::WWW::SOAP11::Error::forbidden()
	}

	# Save DEBUG
	open FILE, ">", "/var/tmp/debug.list.txt";
	print FILE Dumper $list;
	close FILE;	

	#FIXME: noch lange nicht fertig, viel geändert 
	#TODO: was brauchen wir wirklich?
	#TODO: vergl. getList 

	$log->syslog('debug', Dumper $list);
	
	# return
	return { lists => $list };
}




1;
