(function(){
	var email_worker = new Worker('email-worker.js');

	var upload_text_area = document.getElementById('doc_with_emails');
	var upload_button = document.getElementById('trigger_button');
	var total_number = document.getElementById('total_num_emails');

	email_worker.onmessage = (function(e){
		var num_new_emails = parseInt(e.data);
		var total_number_int = parseInt(total_number.innerHTML);
		total_number.innerHTML = (total_number_int + e.data).toString();
	});

	upload_button.onclick = (function(){
		var the_text = upload_text_area.value;
		email_worker.postMessage(the_text);
	});
})();