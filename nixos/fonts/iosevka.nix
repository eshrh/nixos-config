{
  config,
  pkgs,
  ...
}: {
  nixpkgs.config = {
    packageOverrides = super: let
      self = super.pkgs;
    in {
      iosevka-meiseki = self.iosevka.override {
        set = "Meiseki";
        privateBuildPlan = ''
          [buildPlans.IosevkaMeiseki]
          family = "Iosevka Meiseki"
          spacing = "normal"
          serifs = "sans"
          noCvSs = true

          [buildPlans.IosevkaMeiseki.variants.design]
          capital-d = "more-rounded-serifless"
          capital-z = "straight-serifless-with-horizontal-crossbar"
          capital-q = "crossing"
          capital-w = "curly-serifless"
          f = "flat-hook-tailed"
          g = "single-storey-serifless"
          i = "serifed-flat-tailed"
          k = "cursive-bottom-right-serifed"
          l = "serifed-flat-tailed"
          t = "flat-hook-short-neck"
          y = "straight-turn-serifless"
          j = "flat-hook-serifed"
          r = "corner-hooked-serifless"
          two = "straight-neck-serifless"
          four = "closed-serifless"
          seven = "bend-serifless-crossbar"
          eight = "two-circles"
          underscore = "high"
          brace = "straight"
          lower-lambda = "straight-turn"
          zero = "slashed"
          asterisk = "hex-low"
          ampersand = "closed"
          at = "threefold"
          percent = "rings-continuous-slash"
          ascii-single-quote = "straight"
          ascii-grave = "straight"

          [buildPlans.IosevkaMeiseki.ligations]
          inherits = "haskell"
          disables = [
              "center-op-trigger-plus-minus-l",
              "center-op-trigger-plus-minus-r"
          ]
          [buildPlans.IosevkaMeiseki.weights.Light]
          shape = 300
          menu = 300
          css = 300

          [buildPlans.IosevkaMeiseki.weights.Regular]
          shape = 400
          menu = 400
          css = 400

          [buildPlans.IosevkaMeiseki.weights.Bold]
          shape = 700
          menu = 700
          css = 700

          [buildPlans.IosevkaMeiseki.widths.Condensed]
          shape = 500
          menu = 3
          css = "condensed"

          [buildPlans.IosevkaMeiseki.widths.Normal]
          shape = 600
          menu = 5
          css = "normal"
        '';
      };
    };
  };
}
