# ~Talkbot

Urbit's first ever talkbot. Say hello to `~talkbot` in chat! It tries to be useful, but doesn't do the dishes.

**The current code is not reference quality**. It's decent, but uses old syntax and is a bit roundabout in places.

Want to make your own, from scratch? [Urbit By Doing](https://github.com/Fang-/Urbit-By-Doing) can teach you how!

## Functionality

When responding to a message, ~talkbot tries to use the message's audience. For complex audiences this does not yet work well. (It has a special case for the different urbit-meta channels, always sending replies to ~binzod/urbit-meta if the audience includes any official urbit-meta channel.)

* Does various bits of chit-chat.
* Provides page titles and other URL metadata.
* Can perform functionality on command.

### Commands

* `ping` Replies with "Pong."
* `!ping` Measures round-time to caller by using `|hi`.
* `!ignoreme` To have ~talkbot ignore your messages.
* `!unignoreme` (or `~noticeme`) to make ~talkbot respond to your messages again.
* `!chopra` Posts a fake Deepak Chopra quote.
* `!quote` Posts a quote overheard on Urbit.

### Future features

* Automatically post about new issues and pull requests in Urbit repositories.
* Short-form explanations for runes and terms.
* So much more.

## Contributing

Pull requests welcome!

## License

~Talkbot's source code is available under the GNU General Public License v3.0.
