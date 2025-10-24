# Copyright (C) all contributors <meta@public-inbox.org>
# License: GPL-3.0+ <https://www.gnu.org/licenses/gpl-3.0.txt>
#
# Plack/PSGI middleware to reject aggressive bots
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
	$self->app->($env);
}

1;
