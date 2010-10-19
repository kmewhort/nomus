$Id: README.txt,v 1.3 2008/05/27 21:28:03 weitzman Exp $

DESCRIPTION
  Enable feed readers and other simple clients to see certain private pages by providing an authentication token.
  
USAGE
  If you put token=x on the querystring and x is a valid token, then an anonymous user will be authenticated as the user who owns the token. You may learn a user's token by clicking on the tab on the user's profile page.

TODO
  expire tokens after x days - http://drupal.org/node/97820
  start changing links around the site so they have user's token appended automatically
  use the token in other contexts like inbound email (see mailhandler.module)
  
