# somafm.el
A simple somafm interface in emacs

Note: **This is not an official SomaFM project**

I built this for fun because of my love for SomaFM.

![somafm.el screenshot](/screenshot.png "somafm.el screenshot")

## Installation
- Ensure you have `mpv` installed.
  - On macOS: `brew install mpv`

### With use-package & melpa
- ```lisp
  (use-package somafm
    :ensure t)
  ```

### With use-package
- Clone the repository
- Ensure the load-path is properly set based on the directory you cloned to.
  If it was cloned to `~/.emacs.d`, this should work.
  ```lisp
  (use-package somafm
    :load-path "somafm.el")
  ```
### Without use-package
- Clone the repository and make sure it's in your load path.
  ```lisp
  (add-to-list 'load-path "~/.emacs.d/somafm.el")
  ```
- Require the package
  ```lisp
  (require 'somafm)
  ```
- Done!

## Usage
- Launch the soma.fm channel browser via `M-x somafm`
- Use `n` and `p` to navigate
- Press `<return>` to start streaming the highlighted channel
- Press `s` to stop streaming playback
- Press `l` to sort by number of listeners
- Press `g` to refresh the channels list

- The current song will be shown next to the current channel
  - Or you can use `M-x somafm-current-song` to display the current song in the echo area.

- You can add the current song to your favorites file (see
  `somafm-favorites-file`) by calling
  `somafm-add-current-song-to-favorites`, and delete it by calling
  `somafm-delete-current-song-from-favorites`.

## Customization

- You can set the `somafm-sound-quality` custom to 'highest, 'high, or 'low to set the streaming quality.

## Contributing

This project uses
[buttercup](https://github.com/jorgenschaefer/emacs-buttercup) for
tests. To run them:

1. Install [Cask](https://github.com/cask/cask).

2. Run the `cask` command in the project's root directory to install
   dependencies.

3. Execute tests: `cask exec buttercup -L .`
