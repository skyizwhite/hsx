(uiop:define-package #:hsx/builtin
  (:use #:cl)
  (:import-from #:hsx/dsl
                #:deftag))
(in-package #:hsx/builtin)

(defmacro register-builtin-tags (&rest names)
  `(progn
     ,@(mapcan (lambda (name)
                 (list `(deftag ,name)
                       `(export ',name)))
               names)))

;; Built-in tags follow the WHATWG HTML Living Standard element index
;; (https://html.spec.whatwg.org/multipage/indices.html#elements-3),
;; grouped by the standard's "elements of HTML" categories.
(register-builtin-tags
 ;; the root element
 html
 ;; document metadata
 base head link meta style title
 ;; sections
 body address article aside footer header h1 h2 h3 h4 h5 h6 hgroup main
 nav section |search|
 ;; grouping content
 blockquote dd div dl dt figcaption figure hr li menu ol p pre ul
 ;; text-level semantics
 a abbr b bdi bdo br cite code data dfn em i kbd |map| mark q rp rt ruby
 s samp small span strong sub sup |time| u var wbr
 ;; edits
 del ins
 ;; embedded content
 area audio img track video embed iframe object picture source
 ;; tabular data
 caption col colgroup table tbody td tfoot th thead tr
 ;; forms
 button datalist fieldset form input label legend meter optgroup option
 output progress select textarea
 ;; interactive elements
 details dialog summary
 ;; scripting
 canvas noscript script
 ;; web components
 slot template
 ;; SVG — https://www.w3.org/TR/SVG2/eltindex.html
 ;; (a audio canvas iframe script style title video are shared with HTML above)
 svg animate animatemotion animatetransform circle clippath defs desc discard
 ellipse feblend fecolormatrix fecomponenttransfer fecomposite feconvolvematrix
 fediffuselighting fedisplacementmap fedistantlight fedropshadow feflood fefunca
 fefuncb fefuncg fefuncr fegaussianblur feimage femerge femergenode femorphology
 feoffset fepointlight fespecularlighting fespotlight fetile feturbulence filter
 foreignobject g image line lineargradient marker mask metadata mpath path
 pattern polygon polyline radialgradient rect |set| stop switch |symbol| text
 textpath tspan use view
 ;; MathML — https://www.w3.org/TR/mathml-core/
 math annotation annotation-xml maction merror mfrac mi mmultiscripts mn mo mover
 mpadded mphantom mprescripts mroot mrow ms mspace msqrt mstyle msub msubsup msup
 mtable mtd mtext mtr munder munderover semantics
 ;; special HSX tags
 <> raw!)
