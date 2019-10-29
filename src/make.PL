use Module::Build;
my $build = Module::Build->current;

while ( my $output_file = shift ) {
    my $input_file = $output_file . '.in';

    my $sympa_moduledir = $build->notes('sympa-moduledir');
    #$build->notes( 'sympa-modulesubdir'
    my $sympa_execcgidir = $build->notes('sympa-execcgidir');
    my $sympa_piddir = $build->notes('sympa-piddir');
    my $sympa_user = $build->notes('sympa-user');
    my $sympa_group = $build->notes('sympa-group');
    #$build->notes('sympa-sysconfdir');
    my $unitsdir = $build->notes('unitsdir');
    my $perl = $build->notes('perl');

    open(my $in_fh, "<", $input_file) or die "Can't open $input_file for reading: $!";
    open(my $out_fh, ">", $output_file) or die "Can't open $output_file for writing: $!";
    while (<$in_fh>) {
        $_ =~ s/%SYMPA-MODULEDIR%/$sympa_moduledir/g;
        $_ =~ s/%SYMPA-EXECCGIDIR%/$sympa_execcgidir/g;
        $_ =~ s/%SYMPA-PIDDIR%/$sympa_piddir/g;
        $_ =~ s/%SYMPA-USER%/$sympa_user/g;
        $_ =~ s/%SYMPA-GROUP%/$sympa_group/g;
        $_ =~ s/%UNITSDIR%/$unitsdir/g;
        $_ =~ s/%PERL%/$perl/g;
        print $out_fh $_;
    }
    close $out_fh;
    close $in_fh;
}