test:
	ros run --eval "(push #p\"./\" asdf:*central-registry*)" \
	        --eval "(ql:quickload :visp/test)" \
	        --eval "(rove:run :visp/test :style :spec)"