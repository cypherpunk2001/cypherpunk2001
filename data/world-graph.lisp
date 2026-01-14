(:edges
 ((:from :zone-1 :edge :east :to :zone-2 :spawn-edge :west :offset :preserve-y)
  (:from :zone-2 :edge :west :to :zone-1 :spawn-edge :east :offset :preserve-y)
  (:from :zone-1 :edge :south :to :zone-3 :spawn-edge :north :offset :preserve-x)
  (:from :zone-3 :edge :north :to :zone-1 :spawn-edge :south :offset :preserve-x)
  (:from :zone-2 :edge :south :to :zone-4 :spawn-edge :north :offset :preserve-x)
  (:from :zone-4 :edge :north :to :zone-2 :spawn-edge :south :offset :preserve-x)
  (:from :zone-3 :edge :east :to :zone-4 :spawn-edge :west :offset :preserve-y)
  (:from :zone-4 :edge :west :to :zone-3 :spawn-edge :east :offset :preserve-y)))
