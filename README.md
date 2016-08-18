# ~Talkbot

Urbit's first ever talkbot. Say hello to `~talkbot` in chat! Tries to be useful, but doesn't do the dishes.

## Functionality

When responding to a message, ~talkbot tries to use the message's audience. For complex audiences this does not yet work well. (It has a special case for the different urbit-meta channels, always sending replies to ~binzod/urbit-meta if the audience includes any official urbit-meta channel.)

* Does various bits of chit-chat.
* Welcomes users when they join a station.
* Provides titles of Github repositories and issues/pull requests when linked.
* Provides Pastebin paste titles (if they're titled).

### Commands

* `ping` Replies with "Pong."
* `~ignoreme` To have ~talkbot ignore your messages.
* `~unignoreme` (or `~noticeme`) to make ~talkbot respond to your messages again.
* `~whocount` Posts the amount of ships currently in the station.
* `~chopra` Posts a fake Deepak Chopra quote.

### Future features

* Automatically post about new issues and pull requests in Urbit repositories.
* Short-form explanations for runes and terms.
* Talking to users when chat is quiet.

## Contributing

Since ~talkbot is a learning experience for me, I prefer issues/feature requests over pull requests. I won't blindly deny them though!

## License

~Talkbot's source code is available under the GNU General Public License v3.0.
