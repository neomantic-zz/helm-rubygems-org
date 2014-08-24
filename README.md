# helm-rubygems-org.el

A Helm interface to query [rubygems.org](http://rubygems.org)

## Requirements

1. [The helm package](https://github.com/emacs-helm/helm)
2. A rubygems.org account and [API key](http://guides.rubygems.org/rubygems-org-api/#api-authorization)

## Setup

For this helm interface to connect to rubygems.org, API requests must be
authorized using an API key. There are 3 ways to provide this API to
`helm-rubygems-org.el`.

1.  If you have chosen to store your API key at `~.gem/credentials`,
`helm-rubygems-org.el` will recognize it by default.

2. `M-x customize-group` => `helm-rubygems.org` will provide a customization
interface.

3. Using `setq`, bind the var `helm-rubygems-org-api-key` to either the value
of the API key or a file where the API key is store or

## Usage

`M-x helm-rubygems-org` and then type the name of the gem. After a few seconds,
a list of search results should appeare in a helm interface.

*Helm Actions*

By default, hitting the return key on a item in the search results will populate
the kill ring with a string suitable for inclusion an a `Gemfile`. (e.g.
`gem 'guard-rackunit', '~> 1.0.0'`. Alternatively, hitting the tab key will
provide a list of Helm actions.  They are:

1. "Copy Gemfile require" - copies the name and version of the gem to the kill ring.
2. "View Description" - open a buffer with a description of the gem.
3. "Browse source code project" - opens a web browser to the location of the gem's
source code (or if it doesn't exist, open the gems rubygems.org page)
4. "Browse or rubygems.org" - opens a web browser to the rubygems.org page of the
gem


*Important Note*
Connecting to `rubygems.org` takes place through an HTTP connection - which can be
slow. Helm (or emacs) does not provide a mechanism to indicate to the user the progress
of this connection. Consequently, `helm-rubygems-org.el` appears to be doing nothing,
when it actually is. Please be patient, and the list of gem is bound to appear.

## License

[GPLv3](LICENSE)

## Versioning

This library follows [semantic versioning](http://semver.org/) conventions

## Author

[Chad Albers](https://github.com/neomantic)

## Source Code

The source is available on [github.com](https://github.com/neomantic/helm-rubygems-org)

## Issues

Please file bugs and feature requests [here](https://github.com/neomantic/helm-rubygems-org/issues)
