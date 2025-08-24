(ns user
  (:require
   [portal.api :as p]
   [portal.user :as-alias u]))

(p/start {:port 4444})

(.addShutdownHook (Runtime/getRuntime) (Thread. p/close))

(comment
  (p/open {:launcher :auto})
  (add-tap #'p/submit)
  (remove-tap #'p/submit)

  (require '[portal-slides.core :as slides])

  (slides/open {:file "portal_slides/example.md"})
  (slides/open {:mode :boot :file "portal_slides/example.md"})
  (slides/open {:mode :dev :file "portal_slides/example.md"})

  (slides/open
   {;:mode :dev
    :editor :vs-code
    :launcher :vs-code
    :theme ::u/surprising-blueberry
    :file "portal_slides/example.md"})

  comment)