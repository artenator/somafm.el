# somafm.el
A simple somafm interface in emacs

![somafm.el screenshot](/screenshot.png "somafm.el screenshot")

## Installation
- Ensure you have `mpv` installed. 
  - On macOS: `brew install mpv`
- Clone the repository and make sure it's in your load path.
  ```
  (add-to-list 'load-path "~/.emacs.d/somafm.el")
  ```
- Require the package
  ```
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
  
## Customization

- You can set the `somafm-sound-quality` custom to 'highest, 'high, or 'low to set the streaming quality.
