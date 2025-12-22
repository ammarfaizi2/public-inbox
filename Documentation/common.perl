#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use autodie qw(close open seek truncate);
use Fcntl qw(SEEK_SET);
use PublicInbox::Search;
use PublicInbox::IO qw(read_all);
my $addr = 'meta@public-inbox.org';
for my $pod (@ARGV) {
	open my $fh, '+<', $pod;
	my $s = read_all $fh;
	my $orig = $s;
	$s =~ s!^=head1 COPYRIGHT\n.+?^=head1([^\n]+)\n!=head1 COPYRIGHT

Copyright all contributors L<mailto:$addr>

License: AGPL-3.0+ L<https://www.gnu.org/licenses/agpl-3.0.txt>

=head1$1
		!ms;

	$s =~ s!^=head1 CONTACT\n.+?^=head1([^\n]+)\n!=head1 CONTACT

Feedback welcome via plain-text mail to L<mailto:$addr>

The mail archives are hosted at L<https://public-inbox.org/meta/> and
L<http://4uok3hntl7oi7b4uf4rtfwefqeexfzil2w6kgk2jn5z2f764irre7byd.onion/meta/>

=head1$1
		!ms;
	$s =~ s!^=for\scomment\n
			^AUTO-GENERATED-SEARCH-TERMS-BEGIN\n
			.+?
			^=for\scomment\n
			^AUTO-GENERATED-SEARCH-TERMS-END\n
			!search_terms()!emsx;
	$s =~ s/[ \t]+$//sgm;
	if ($s eq $orig) {
		my $t = time;
		utime($t, $t, $fh);
	} else {
		seek $fh, 0, SEEK_SET;
		truncate $fh, 0;
		print $fh $s;
		close $fh;
	}
}

sub search_terms {
	my $s = PublicInbox::Search::help2txt(@PublicInbox::Search::HELP);
	$s =~ s/^  //gms;
	substr($s, 0, 0, "=for comment\nAUTO-GENERATED-SEARCH-TERMS-BEGIN\n\n");
	$s .= "\n=for comment\nAUTO-GENERATED-SEARCH-TERMS-END\n";
}
