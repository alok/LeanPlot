import ProofWidgets.Data.Html
import Lean.Server.Rpc.Basic

namespace LeanPlot

open ProofWidgets Lean Server

/-- Props for the WarningBanner component. -/
structure WarningBannerProps where
  /-- The warning message to display. -/
  message : String
  deriving RpcEncodable

/-- A simple component to display a warning message in a banner. -/
-- Removing @[widget_module] and javascript as an experiment.
-- If the component only provides a `view : Props → Html`,
-- ProofWidgets might handle rendering without custom JS for this specific component.
def WarningBanner (props : WarningBannerProps) : Html :=
  .element "div"
    #[("style", json% {
        backgroundColor: "orange",
        color: "black",
        padding: "10px",
        borderRadius: "5px",
        marginTop: "5px",
        marginBottom: "5px"
    })]
    #[.element "strong" #[] #[.text "Warning: "], .text props.message]

end LeanPlot
