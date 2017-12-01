# clj-retry

A Clojure library designed to safely retry on command according to a plan

## Usage

(safe (fn [] do-dangerous-thing)
      (with-plan :handler (fn [ex] (handle-ex ex))  :max-retries 3 :delay 10))

## License

Copyright © 2017

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
