# Copyright (C) all contributors <meta@public-inbox.org>
# License: GPL-3.0+ <https://www.gnu.org/licenses/gpl-3.0.txt>
#
# Plack/PSGI middleware to reject aggressive scrapers, requires
# public-inbox-(httpd|netd) to detect persistent connections
# via $env->{'pi-httpd.request_nr'}.
package RejectBots;
use v5.12;
use parent qw(Plack::Middleware);

my $bad_ua = join '|',
      'Bytespider', 'meta-externalagent', 'petalbot',
      'dataforseo', 'mj12bot', 'yandex', 'zoominfobot',
      'amazonbot', 'barkrowler', 'oai-searchbot', 'chatgpt',
      'semrushbot', 'ahrefsbot', 'alibaba',
      'gptbot', 'awario.*bot', 'magesiftbot', 'serpstatbot',
      'claudebot', 'google-extended', 'seekport crawler',
      'blexbot', 'turnitin', 'Scrapy', 'bingbot';
$bad_ua = qr/(?:$bad_ua)/i;

sub call {
	my ($self, $env) = @_;
	my $ua = $env->{HTTP_USER_AGENT} // '';
	return [ 403, [], [] ] if $ua =~ /$bad_ua/o;
	my $res = $self->app->($env);
	my $uri;
	if ($env->{PATH_INFO} !~ /\.css\z/ &&
			$ua =~ m!\A(?:Mozilla|Opera)/! &&
			defined($uri = $env->{REQUEST_URI}) &&
			($env->{HTTP_REFERER} // '') !~ /\Q$uri\E\z/ &&
			!$env->{'pi-httpd.request_nr'}) {
		my $body = <<EOM;
Requiring persistent connection to access: $uri ...
EOM
		[ 200, [ 'Refresh' => 1,
			'Content-Length' => length($body) ], [ $body ] ]
	} else {
		$res;
	}
}

1;
