<h1 style="border: none; font-size: 72px">
Portal Slides
</h1>
<h2 style="border: none; font-size: 32px; color: white">
by Clojure Fan
</h2>

---

# Example Slide 1

Code to eval

```clojure
(+ 1 2 3)
```

```clojure
(range 100)
```

---

# Example Slide 2

Open Portal

```clojure
(require '[portal.api :as p])
(p/open)
```

```clojure
(add-tap #'p/submit)
```

```clojure
(tap> :hello/world)
```

---

# Example Slide 3

```clojure
(require '[portal.viewer :as v])
```

```clojure
(v/edn (slurp "deps.edn"))
```