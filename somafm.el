;; somafm.el --- A soma.fm interface for emacs -*- lexical-binding: t -*-
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

(defvar somafm-channels nil)

(defvar somafm-original-channel-order '())

(defvar somafm-current-channel-order '())

(defvar somafm-icons '())

(defvar somafm-current-channel nil)

(defvar somafm-currently-sorted nil)

(defvar somafm-current-song nil)

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
  (car (seq-filter (-lambda ((&plist :id id))
                     (string-equal id channel-id))
                   channel-list)))

(defun somafm--insert-channels ()
  "Insert a formatted version of all channels in the current channel list in the current buffer."
  (dolist (channel-id somafm-current-channel-order)
    (somafm--insert-channel (somafm--get-channel-by-id somafm-channels channel-id))))

(defun somafm--show-channels-buffer ()
  "Get or create the main somafm channels buffer, and insert the formatted channels list."
  (let ((somafm-buffer (get-buffer-create "*somafm channels*"))
        (inhibit-read-only t))
    (with-current-buffer somafm-buffer
      (switch-to-buffer (current-buffer))
      (erase-buffer)
      (somafm--insert-channels)
      (read-only-mode)
      (somafm-mode)
      (goto-char (point-min))
      (when (search-forward "►" nil t)
        (beginning-of-line)))))

(defun somafm--insert-image (bytes)
  "Given a string representation BYTES of bytes, give the proper image encoding and insert an image at point."
  (-> bytes
      (encode-coding-string 'binary)
      (create-image nil t)
      (insert-image)))

(defun somafm--image-parser ()
  "Image parser for http requests."
  (buffer-substring (point-min) (point-max)))

(defun somafm--refresh-channels ()
  "Refresh the channels by sending a request to the soma.fm API, and retrieve all the channel images."
  (interactive)
  (request
    somafm-channels-url
    :parser 'somafm--http-parser
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((channels (plist-get data :channels))
                       (retrieved-channel-order (mapcar (-lambda ((&plist :id id))
                                                          id)
                                                        channels)))
                  (setq somafm-channels channels)
                  (setq somafm-original-channel-order retrieved-channel-order)
                  (when (not somafm-current-channel-order)
                    (setq somafm-current-channel-order retrieved-channel-order))
                  (somafm--refresh-icons))))))

(defun somafm--refresh-icons ()
  "Send requests to retrieve the images for each channel in SOMAFM-CHANNELS."
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
                      (when (>= count max-count)
                        (somafm--show-channels-buffer)))))))))

(defun somafm--get-url-from-quality (stream-urls given-quality)
  "Given a list of urls STREAM-URLS and a quality setting GIVEN-QUALITY, return the URL with the matching desired quality setting."
  (car (seq-filter (-lambda ((&plist :quality quality))
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
    (kill-line)))

(defun somafm--insert-current-song (current-song)
  "Insert the current song CURRENT-SONG at point in the current buffer."
  (insert " " (somafm--format-current-song current-song)))

(defun somafm--update-current-song (current-song)
  "Update the channels buffer with the current song CURRENT-SONG, based on the output of the somafm player process."
  (let ((channels-buf (get-buffer-create "*somafm channels*"))
        (inhibit-read-only t))
    (setq somafm-current-song current-song)
    (with-current-buffer channels-buf
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "►" nil t)
          (somafm--clear-rest-of-line)
          (somafm--insert-current-song current-song))
        (read-only-mode)))))

(defun somafm--play ()
  "Play the currently selected channel at point."
  (interactive)
  (let* ((channel-ol (somafm--get-overlay-by "somafm-channel"))
         (stream-urls (overlay-get channel-ol 'stream-urls))
         (id (overlay-get channel-ol 'id)))
    (somafm--stop)
    (setq somafm-current-channel id)
    (let ((player-proc (start-process-shell-command "somafm player" "*somafm player*"
                                                    (format "mpv %s 2> /dev/null"
                                                            (plist-get (somafm--quality-handler stream-urls somafm-sound-quality) :url)))))
      (set-process-filter player-proc (lambda (_ output)
                                        (when (string-match "title: \\(.*\\)" output)
                                          (somafm--update-current-song (match-string 1 output)))))
      (somafm--show-channels-buffer))))

(defun somafm--stop ()
  "Stop streaming the channel that is currently playing."
  (interactive)
  (-when-let (player-proc (get-process "somafm player"))
    (delete-process player-proc)
    (setq somafm-current-channel nil)
    (setq somafm-current-song nil)
    (with-current-buffer (get-buffer-create "*somafm channels*")
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (search-forward "►" nil t)
          (delete-char -1)
          (kill-line))))))

(defun somafm--sort ()
  "Sort the channels list view, or unsort it if the list is already sorted."
  (interactive)
  (if (not somafm-currently-sorted)
      (progn
        (setq somafm-current-channel-order
              (->> somafm-channels
                   (seq-sort-by (-lambda ((&plist :listeners listeners))
                                  (string-to-number listeners))
                                #'>)
                   (mapcar (-lambda ((&plist :id id))
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

(defun somafm ()
  "Refresh channels if we don't have the list already, otherwise show the channel buffer."
  (interactive)
  (if (not somafm-channels)
      (somafm--refresh-channels)
    (somafm--show-channels-buffer)))

(provide 'somafm)
;;; somafm.el ends here
