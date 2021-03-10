(require 'somafm)

(describe "The 'favorites' feature"

  (describe "when searching for a favorite"
    (before-all
      (setq somafm-favorites-file (prepare-favorites-file "song 1\nSONG 2\nsong 3 (remix)\nsong 4 [^$*]\n")))

    (it "should find it if exists"
      (expect (somafm--favorite-p "song 1") :to-be-truthy))

    (it "should find it even if stored in a different case"
      (expect (somafm--favorite-p "song 2") :to-be-truthy))

    (it "should not find it from partial match"
      (expect (somafm--favorite-p "song 3") :not :to-be-truthy))

    (it "should find it even if special characters"
      (expect (somafm--favorite-p "song 4 [^$*]") :to-be-truthy)))

  (describe "when adding a favorite"
    (before-each
      (spy-on 'insert :and-call-through)
      (setq somafm-favorites-file (prepare-favorites-file "song 1\nSONG 2\nsong 3 (remix)\n")))

    (it "should add it if not already present"
      (let* ((current-song "current artist - current song")
             (somafm-current-song current-song))
        (expect (somafm--favorite-p somafm-current-song) :not :to-be-truthy)
        (somafm-add-current-song-to-favorites)
        (expect 'insert :to-have-been-called-with (concat current-song "\n"))
        (expect (somafm--favorite-p somafm-current-song) :to-be-truthy)))

    (it "should not add it if already present"
      (let* ((current-song "song 1")
             (somafm-current-song current-song))
        (somafm-add-current-song-to-favorites)
        (expect 'insert :not :to-have-been-called)))

    (it "should add it even if a partial match exists"
      (let ((somafm-current-song "song 1 (remix)"))
        (somafm-add-current-song-to-favorites)
        (expect 'insert :to-have-been-called-with "song 1 (remix)\n"))))

  (describe "when deleting a favorite"
    (before-each
      (spy-on 'flush-lines :and-call-through)
      (setq somafm-favorites-file (prepare-favorites-file "song 1\n")))

    (it "should delete it if already present"
      (let ((somafm-current-song "song 1"))
        (expect (somafm--favorite-p somafm-current-song) :to-be-truthy)
        (somafm-delete-current-song-from-favorites)
        (expect 'flush-lines :to-have-been-called-with "^song 1$")
        (expect (somafm--favorite-p somafm-current-song) :not :to-be-truthy)))

    (it "should do nothing if not a favorite"
      (let ((somafm-current-song "song 4"))
        (somafm-add-current-song-to-favorites)
        (expect 'flush-lines :not :to-have-been-called)))))

(defun prepare-favorites-file (file-content)
  "Write FILE-CONTENT to a temporary file and return that file."
  (let ((test-file (make-temp-file "somafm-test-")))
    (write-region file-content nil test-file)
    test-file))

;; Local Variables:
;; eval: (when (fboundp 'buttercup-minor-mode) (buttercup-minor-mode))
;; End:
