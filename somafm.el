;;; -*- lexical-binding: t -*-
;; somafm --- 1password for emacs
;;; Commentary:
(require 'url)
(require 'json)
(require 'cl-lib)
(require 'dash)

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

(setq somafm-channels nil)
(setq somafm-icons nil)
(setq somafm-original-channel-order '()
      somafm-current-channel-order '())

(defun somafm--get-in-plist (plist &rest keys)
  (while keys
    (setq plist (plist-get plist (car keys)))
    (setq keys (cdr keys)))
  plist)

(defun somafm--create-overlay-type (type start-point props)
  (let ((ol (make-overlay start-point (point))))
    (overlay-put ol 'somafm-type type)
    (dolist (prop props)
      (overlay-put ol (car prop) (car (last prop))))
    ol))

(defun somafm--get-overlay-by (s &optional pos)
  (let ((all-overlays (overlays-at (or pos (point)))))
    (car (seq-filter (lambda (ol)
		       (let ((ol-type (overlay-get ol 'somafm-type)))
			 (and ol-type (string-match-p s ol-type))))
		     all-overlays))))

(defun somafm--http-parser ()
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read)))

(defun somafm--insert-channel (channel)
  (-let (((&plist :title title :id id :genre genre :listeners listeners :playlists playlists) channel)
         (somafm-channel-start (point)))
    (somafm--insert-image (plist-get somafm-icons (intern id)))
    (insert (format " %s genre:%s listeners: %s " title genre listeners))
    (when (string-equal id somafm-current-channel)
      (insert (propertize "►" 'font-lock-face '(:height 5))))
    (insert "\n")
    (somafm--create-overlay-type "somafm-channel" somafm-channel-start
                                 `((begin-content ,somafm-channel-start)
                                   (stream-urls ,playlists)
                                   (id ,id)))))

(defun somafm--get-channel-by-id (channel-list channel-id)
  (car (seq-filter (-lambda ((&plist :id id))
                     (string-equal id channel-id))
                   channel-list)))

(defun somafm--insert-channels ()
  (dolist (channel-id somafm-current-channel-order)
    (somafm--insert-channel (somafm--get-channel-by-id somafm-channels channel-id))))

(defun somafm--show-channels-buffer ()
  (let ((somafm-buffer (get-buffer-create "*somafm channels*"))
        (inhibit-read-only t))
    (let ((saved-pos (point)))
      (with-current-buffer somafm-buffer
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (somafm--insert-channels)
        (read-only-mode)
        (somafm-mode)
        (goto-char saved-pos)))))

(defun somafm--insert-image (bytes)
  (-> bytes
      (encode-coding-string 'binary)
      (create-image nil t)
      (insert-image)))

(defun somafm--image-parser ()
  (buffer-substring (point-min) (point-max)))

(defun somafm--refresh-channels ()
  (interactive)
  (url-retrieve
   somafm-channels-url
   (lambda (status)
     (goto-char (point-min))
     (search-forward "\n\n" nil t)
     (let* ((json-object-type 'plist)
            (json-array-type 'list)
            (data (json-read))
            (channels (plist-get data :channels))
            (retrieved-channel-order (mapcar (-lambda ((&plist :id id))
                                               id)
                                             channels)))
       (setq somafm-channels channels)
       (setq somafm-original-channel-order retrieved-channel-order)
       (when (not somafm-current-channel-order)
         (setq somafm-current-channel-order retrieved-channel-order))
       (somafm--refresh-icons)))))

(defun somafm--refresh-icons ()
  (let ((count 0)
        (max-count (length somafm-channels)))
    (dolist (channel somafm-channels)
      (-let (((&plist :id id :image image) channel))
        (url-retrieve
         image
         (lambda (status)
           (goto-char (point-min))
           (search-forward "\n\n" nil t)
           (setq somafm-icons (plist-put somafm-icons (intern id) (buffer-substring (point) (point-max))))
           (setq count (1+ count))
           (when (>= count max-count)
             (somafm--show-channels-buffer))))))))

(defun somafm--get-url-from-quality (stream-urls given-quality)
  (car (seq-filter (-lambda ((&plist :quality quality))
                     (string-equal quality given-quality))
                   stream-urls)))

(defun somafm--quality-handler (stream-urls quality)
  (pcase quality
    ('highest (somafm--get-url-from-quality stream-urls "highest"))
    ('high (somafm--get-url-from-quality stream-urls "high"))
    ('low (somafm--get-url-from-quality stream-urls "low"))))

(defun somafm--play ()
  (interactive)
  (let* ((channel-ol (somafm--get-overlay-by "somafm-channel"))
         (stream-urls (overlay-get channel-ol 'stream-urls))
         (id (overlay-get channel-ol 'id)))
    (somafm--stop)
    (setq somafm-current-channel id)
    (start-process-shell-command "somafm player" "*somafm player*"
                                 (format "mpv %s 2> /dev/null"
                                         (plist-get (somafm--quality-handler stream-urls somafm-sound-quality) :url)))
    (somafm--show-channels-buffer)))

(defun somafm--stop ()
  (interactive)
  (when-let ((player-proc (get-process "somafm player")))
    (delete-process player-proc)
    (setq somafm-current-channel nil)
    (somafm--show-channels-buffer)))

(defun somafm--sort ()
  (interactive)
  (if (not somafm-currently-sorted)
      (setq somafm-current-channel-order
            (->> somafm-channels
                 (seq-sort-by (-lambda ((&plist :listeners listeners))
                                (string-to-number listeners))
                              #'>)
                 (mapcar (-lambda ((&plist :id id))
                           id))))
    (setq somafm-current-channel-order somafm-original-channel-order))
  (setq somafm-currently-sorted (not somafm-currently-sorted))
  (somafm--show-channels-buffer))

(defun somafm ()
  (interactive)
  (if (not somafm-channels)
      (somafm--refresh-channels)
    (somafm--show-channels-buffer)))

(provide 'somafm)
;;; somafm.el ends here
