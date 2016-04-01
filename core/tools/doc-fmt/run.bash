#!/bin/bash

if ! [ -d "./.git" ]
	then
		echo "Should be executed from the repo root."
		exit 1
fi

#rm #+HTML_HEAD_EXTRA: ... readtheorg.css" /> in the doc.
find ./doc    -name "*.org" -type f -exec gsed -i '/#+HTML_HEAD_EXTRA.*readtheorg.css.*/d' {} \;

#rm #+HTML_HEAD_EXTRA: ... readtheorg.css" /> in the layers.
find ./layers -name "*.org" -type f -exec gsed -i '/#+HTML_HEAD_EXTRA.*readtheorg.css.*/d' {} \;



#replace :TOC_4_org: with :TOC_4_gh: in the doc.
find ./doc    -name "*.org" -type f -exec gsed -i 's/:TOC_4_org:/:TOC_4_gh:/' {} \;

#replace :TOC_4_org: with :TOC_4_gh: in the layers.
find ./layers -name "*.org" -type f -exec gsed -i 's/:TOC_4_org:/:TOC_4_gh:/' {} \;



#apply toc-org to doc.
find ./doc    -name "*.org" -type f -exec emacs -batch -l ./core/tools/doc-fmt/toc-org-apply.el '{}' -f toc-apply \;

#apply toc-org to layers.
find ./layers -name "*.org" -type f -exec emacs -batch -l ./core/tools/doc-fmt/toc-org-apply.el '{}' -f toc-apply \;
