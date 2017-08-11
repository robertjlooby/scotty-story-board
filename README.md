# scotty-story-board

A project management tool built using [Scotty](https://github.com/scotty-web/scotty)

## Setup

You'll need:

* [PostgreSQL](https://www.postgresql.org/)
* [Stack](https://docs.haskellstack.org/en/stable/README/)

### Running

* Copy the development config example: `cp config/development.env.example config/development.env`
* Fill in API tokens
* Create the database:
  * `createuser --createdb scotty`
  * `createdb --owner=scotty scotty-story-board`
* Build: `stack build`
* Migrate the database: `stack exec migrate development`
* Run: `stack exec web`
* Visit [localhost:3000](http://localhost:3000)

### Running tests

* Create the database:
  * `createuser --createdb scotty` (if not done already)
  * `createdb --owner=scotty scotty-story-board-test`
* Migrate the database: `stack exec migrate test`
* Build and run: `stack test`
