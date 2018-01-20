(function(){
	var email_regex = /(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))/g;
	onmessage = function(e){  // set function to run whenever the worker receives a message
		var uploaded_doc = e.data;
		console.log('Received ' + uploaded_doc);

		var matches = uploaded_doc.match(email_regex);
		console.log('Matches: ' + matches.toString());
		num_emails = matches.length;
		postMessage(num_emails);  // post number of email matches to parent javascript process
	}
})();
