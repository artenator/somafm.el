;;; somafm.el --- A simple soma.fm interface -*- lexical-binding: t -*-

;; Author: Arte Ebrahimi <>
;; Keywords: multimedia
;; Package-Requires: ((emacs "26.1") (dash "2.12.0") (request "0.3.2") (cl-lib "0.6.1"))
;; URL: https://github.com/artenator/somafm.el
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; somafm.el brings a simple user interface for soma.fm to Emacs.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'dash)
(require 'request)

(defconst somafm-mode-map
  (let ((keymap (make-keymap)))
    (define-key keymap "n" 'next-line)
    (define-key keymap "p" 'previous-line)
    (define-key keymap (kbd "RET") 'somafm--play)
    (define-key keymap (kbd "<return>") 'somafm--play)
    (define-key keymap "g" 'somafm--refresh-channels)
    (define-key keymap "s" 'somafm--stop)
    (define-key keymap "l" 'somafm--sort)
    keymap))

(define-derived-mode somafm-mode special-mode "somafm-mode"
  "somafm - somafm mode
   \\{somafm-mode-map}")

(defcustom somafm-channels-url "https://somafm.com/channels.json"
  "URL to retrieve channels information from."
  :group 'somafm
  :type 'string)

(defcustom somafm-sound-quality 'highest
  "Sound quality for playback."
  :group 'somafm
  :type 'symbol
  :options '(highest high low))

(defcustom somafm-player-command "mpv"
  "Command for the media player."
  :group 'somafm
  :type 'string)

(defcustom somafm-refresh-interval 300
  "Number of seconds needed before the channels are automatically refreshed."
  :group 'somafm
  :type 'integer)

(defvar somafm-test (float-time))

(defvar somafm-channels nil)

(defvar somafm-original-channel-order '())

(defvar somafm-current-channel-order '())

(defvar somafm-icons '())

(defvar somafm-current-channel nil)

(defvar somafm-currently-sorted nil)

(defvar somafm-current-song nil)

(defvar somafm-last-refresh-time 0)

(defun somafm--get-in-plist (plist &rest keys)
  "A helper function that gets a value at KEYS from a nested PLIST."
  (while keys
    (setq plist (plist-get plist (car keys)))
    (setq keys (cdr keys)))
  plist)

(defun somafm--create-overlay-type (type start-point props)
  "Create an overlay with type TYPE and at starting point START-POINT until current position of point, with the properties PROPS."
  (let ((ol (make-overlay start-point (point))))
    (overlay-put ol 'somafm-type type)
    (dolist (prop props)
      (overlay-put ol (car prop) (car (last prop))))
    ol))

(defun somafm--get-overlay-by (s &optional pos)
  "Get an overlay of type S at the current point or at POS."
  (let ((all-overlays (overlays-at (or pos (point)))))
    (car (seq-filter (lambda (ol)
		       (let ((ol-type (overlay-get ol 'somafm-type)))
			 (and ol-type (string-match-p s ol-type))))
		     all-overlays))))

(defun somafm--http-parser ()
  "JSON parser for http requests."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read)))

(defun somafm--insert-channel (channel)
  "Insert CHANNEL in a formatted structure into the current buffer."
  (-let (((&plist :title title :id id :genre genre :listeners listeners :playlists playlists) channel)
         (somafm-channel-start (point)))
    (somafm--insert-image (plist-get somafm-icons (intern id)))
    (insert (format " %s genre:%s listeners: %s " title genre listeners))
    (when (string-equal id somafm-current-channel)
      (insert (propertize "►" 'font-lock-face '(:height 5)))
      (when somafm-current-song
        (somafm--insert-current-song somafm-current-song)))
    (insert "\n")
    (somafm--create-overlay-type "somafm-channel" somafm-channel-start
                                 `((begin-content ,somafm-channel-start)
                                   (stream-urls ,playlists)
                                   (id ,id)))))

(defun somafm--get-channel-by-id (channel-list channel-id)
  "Given a CHANNEL-ID, get the specific channel from CHANNEL-LIST."
  (->> channel-list
       (seq-filter (-lambda ((&plist :id))
                     (string-equal id channel-id)))
       (car)))

(defun somafm--insert-channels ()
  "Insert a formatted version of all channels in the current channel list in the current buffer."
  (dolist (channel-id somafm-current-channel-order)
    (somafm--insert-channel (somafm--get-channel-by-id somafm-channels channel-id))))

(defun somafm--redraw-channels-buffer ()
  (let ((inhibit-read-only t))
    (-when-let (somafm-buffer (get-buffer "*somafm channels*"))
      (with-current-buffer somafm-buffer
        (erase-buffer)
        (somafm--insert-channels)
        (read-only-mode)
        (somafm-mode)))))

(defun somafm--goto-current-song ()
  (-when-let (channels-buffer (get-buffer "*somafm channels*"))
    (with-current-buffer channels-buffer
      (goto-char (point-min))
      (when (search-forward "►" nil t)
        (beginning-of-line)))))

(defun somafm--show-channels-buffer ()
  "Get or create the main somafm channels buffer, and insert the formatted channels list."
  (let ((somafm-buffer (get-buffer-create "*somafm channels*")))
    (with-current-buffer somafm-buffer
      (switch-to-buffer (current-buffer))
      (somafm--redraw-channels-buffer)
      (somafm--goto-current-song))))

(defun somafm--insert-image (bytes)
  "Given a string representation BYTES of bytes, give the proper image encoding and insert an image at point."
  (-> bytes
      (encode-coding-string 'binary)
      (create-image nil t)
      (insert-image)))

(defun somafm--image-parser ()
  "Image parser for http requests."
  (buffer-substring (point-min) (point-max)))

(defun somafm--refresh-channels (&optional on-success)
  "Refresh the channels by sending a request to the soma.fm API, and retrieve all the channel images.  Call ON-SUCCESS function if provided."
  (interactive)
  (request
    somafm-channels-url
    :parser 'somafm--http-parser
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((channels (plist-get data :channels))
                       (retrieved-channel-order (mapcar (-lambda ((&plist :id))
                                                          id)
                                                        channels)))
                  (setq somafm-channels channels)
                  (setq somafm-original-channel-order retrieved-channel-order)
                  (setq somafm-last-refresh-time (float-time))
                  (unless somafm-current-channel-order
                    (setq somafm-current-channel-order retrieved-channel-order))
                  (somafm--refresh-icons on-success))))))

(defun somafm--refresh-icons (&optional on-success)
  "Send requests to retrieve the images for each channel in SOMAFM-CHANNELS.  Takes an optional function ON-SUCCESS that is triggered when all the icons have been retrieved."
  (let ((count 0)
        (max-count (length somafm-channels)))
    (dolist (channel somafm-channels)
      (-let (((&plist :id id :image image) channel))
        (request
          image
          :parser 'somafm--image-parser
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (setq somafm-icons (plist-put somafm-icons (intern id) data))
                      (setq count (1+ count))
                      (when (and on-success (>= count max-count))
                        (funcall on-success)))))))))

(defun somafm--get-url-from-quality (stream-urls given-quality)
  "Given a list of urls STREAM-URLS and a quality setting GIVEN-QUALITY, return the URL with the matching desired quality setting."
  (car (seq-filter (-lambda ((&plist :quality))
                     (string-equal quality given-quality))
                   stream-urls)))

(defun somafm--quality-handler (stream-urls quality)
  "Given a list of stream urls STREAM-URLS, and desired quality QUALITY, pass to the filter function to retrieve the url."
  (pcase quality
    ('highest (somafm--get-url-from-quality stream-urls "highest"))
    ('high (somafm--get-url-from-quality stream-urls "high"))
    ('low (somafm--get-url-from-quality stream-urls "low"))))

(defun somafm--format-current-song (current-song)
  "Formats the current song CURRENT-SONG to be wrapped with parentheses."
  (format "(%s)" current-song))

(defun somafm--clear-rest-of-line ()
  "Kill line if we are not at the end of the line already."
  (unless (char-equal (char-after (point)) ?\n)
    (delete-region (point) (line-end-position))))

(defun somafm--insert-current-song (current-song)
  "Insert the current song CURRENT-SONG at point in the current buffer."
  (insert " " (somafm--format-current-song current-song)))

(defun somafm--update-current-song (current-song)
  "Update the channels buffer with the current song CURRENT-SONG, based on the output of the somafm player process."
  (let ((inhibit-read-only t))
    (setq somafm-current-song current-song)
    (-when-let (channels-buf (get-buffer "*somafm channels*"))
      (with-current-buffer channels-buf
        (save-excursion
          (goto-char (point-min))
          (when (search-forward "►" nil t)
            (somafm--clear-rest-of-line)
            (somafm--insert-current-song current-song))
          (read-only-mode))))))

(defun somafm--play-by-channel-id (channel-id)
  (setq somafm-current-channel channel-id)
  (-let* (((&plist :playlists stream-urls) (somafm--get-channel-by-id somafm-channels channel-id))
          (player-proc (start-process-shell-command "somafm player"
                                                    "*somafm player*"
                                                    (format "%s %s 2> /dev/null"
                                                            somafm-player-command
                                                            (plist-get (somafm--quality-handler stream-urls somafm-sound-quality)
                                                                       :url)))))
    (set-process-filter player-proc (lambda (_ output)
                                      (when (string-match "title: \\(.*\\)" output)
                                        (somafm--update-current-song (match-string 1 output)))))))

(defun somafm--play ()
  "Play the currently selected channel at point."
  (interactive)
  (let* ((channel-ol (somafm--get-overlay-by "somafm-channel"))
         (stream-urls (overlay-get channel-ol 'stream-urls))
         (id (overlay-get channel-ol 'id)))
    (somafm--stop)
    (somafm--play-by-channel-id id)
    (somafm--show-channels-buffer)))

(defun somafm--stop ()
  "Stop streaming the channel that is currently playing."
  (interactive)
  (-when-let (player-proc (get-process "somafm player"))
    (delete-process player-proc)
    (setq somafm-current-channel nil)
    (setq somafm-current-song nil)
    (-when-let (channels-buf (get-buffer "*somafm channels*"))
      (with-current-buffer channels-buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (when (search-forward "►" nil t)
              (delete-char -1)
              (somafm--clear-rest-of-line))))))))

(defun somafm--sort ()
  "Sort the channels list view, or unsort it if the list is already sorted."
  (interactive)
  (if (not somafm-currently-sorted)
      (progn
        (setq somafm-current-channel-order
              (->> somafm-channels
                   (seq-sort-by (-lambda ((&plist :listeners))
                                  (string-to-number listeners))
                                #'>)
                   (mapcar (-lambda ((&plist :id))
                             id))))
        (setq somafm-currently-sorted t))
    (setq somafm-current-channel-order somafm-original-channel-order)
    (setq somafm-currently-sorted nil))
  (somafm--show-channels-buffer)
  (move-beginning-of-line nil))

(defun somafm-current-song ()
  "Display the current song in the echo area."
  (interactive)
  (when somafm-current-song
    (message "Somafm current song: %s" somafm-current-song)))

(defun somafm--refresh-time-elapsed-p ()
  "Determines whether the refresh threshold has been reached."
  (let ((seconds-since-last-refresh (->> somafm-last-refresh-time
                                         (time-subtract nil)
                                         (float-time))))
    (> seconds-since-last-refresh somafm-refresh-interval)))

(defun somafm ()
  "Refresh channels if we don't have the list already, or if the refresh interval has passed, otherwise show the channel buffer."
  (interactive)
  (if (or (not somafm-channels) (somafm--refresh-time-elapsed-p))
      (somafm--refresh-channels t)
    (somafm--show-channels-buffer)))

(defun somafm--completing-read ()
  (let* ((channels-kv (mapcar (lambda (channel-id)
                                (-let (((&plist :title :id) (somafm--get-channel-by-id somafm-channels channel-id)))
                                  (cons title id)))
                              somafm-current-channel-order))
         (channel-id (-> (completing-read "Pick a channel:" channels-kv)
                         (assoc channels-kv)
                         (cdr))))
    (somafm--stop)
    (somafm--play-by-channel-id channel-id)
    (somafm--redraw-channels-buffer)
    (somafm--goto-current-song)))

(defun somafm-by-completion ()
  (interactive)
  (if (or (not somafm-channels) (somafm--refresh-time-elapsed-p))
      (somafm--refresh-channels 'somafm--completing-read)
    (somafm--completing-read)))

(provide 'somafm)
;;; somafm.el ends here
