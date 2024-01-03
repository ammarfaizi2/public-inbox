# Copyright all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# represents a Maildir, MH or IMAP "watch" item
package PublicInbox::LeiWatch;
use v5.12;
use parent qw(PublicInbox::IPC);

# "url" may be something like "maildir:/path/to/dir" or "mh:/path/to/dir"
sub new { bless { url => $_[1] }, $_[0] }

1;
