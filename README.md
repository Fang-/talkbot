# ~Talkbot

Urbit's first ever talkbot. Say hello to `~talkbot` in chat! It tries to be useful, but doesn't do the dishes.  
It relies on the unofficial [stringlib](https://github.com/fang-/urbit-string).

**The current code is not reference quality**. A cleanup of this mess is planned, but there's no ETA.

Want to make your own, from scratch? [Urbit By Doing](https://github.com/Fang-/Urbit-By-Doing) can teach you how!

## Functionality

When responding to a message, ~talkbot tries to use the message's audience. For complex audiences this does not yet work well. (It has a special case for the different urbit-meta channels, always sending replies to ~binzod/urbit-meta if the audience includes any official urbit-meta channel.)

* Does various bits of chit-chat.
* Provides titles of Github repositories and issues/pull requests when linked.
* Provides Pastebin paste titles (if they're titled).
* Logs station activity.
* Can perform functionality on command.

### Commands

* `ping` Replies with "Pong."
* `~talkping` Measures round-time to station by sending a message.
* `~myping` Measures round-time to caller by using `|hi`.
* `~ignoreme` To have ~talkbot ignore your messages.
* `~unignoreme` (or `~noticeme`) to make ~talkbot respond to your messages again.
* `~whocount` Posts the amount of *active* ships currently in the station.
* `~chopra` Posts a fake Deepak Chopra quote.

### Future features

* Automatically post about new issues and pull requests in Urbit repositories.
* Short-form explanations for runes and terms.
* Talking to users when chat is quiet.
* So much more.

## Contributing

Since ~talkbot is a learning experience for me, I prefer issues/feature requests over pull requests. I won't blindly deny them though!

## License

~Talkbot's source code is available under the GNU General Public License v3.0.
