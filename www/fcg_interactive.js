/* This file contains prototype code that was developed for research purposes and should not be used in production environments.
   No warranties are provided. */



/* Change this to the correct url for the fcg-server */
var comprehension_route = "http://127.0.0.1:1170/comprehend-utterance-fcg-interactive";
var production_route = "http://127.0.0.1:1170/produce-utterance-fcg-interactive";
var examples_route = "http://127.0.0.1:1170/examples-fcg-interactive";

/* Set maximum timeouts */
var comprehension_timeout = 5;
var production_timeout = 5;

/* Global variables */
var chosenGrammar = "";
var package = "";
var utterance = "";
var meaning = "";
resetInterface();

/* Choose grammar and set global variables accordingly, add cases here
to include additionnal grammars. */

function grammarChosen(value) {
	switch (value) {
		case "english":
		chosenGrammar = "*fcg-constructions*";
		package = "fcg";
		break;
	}
	loadMeanings();
	loadUtterances();
}

/* Comprehension */

function comprehend() {
	if (chosenGrammar) {
		utterance = document.getElementById("utterance").value;
		if (utterance) {
			/* Reset meaning related fields */
			document.getElementById("meaning").value = "";
			document.getElementById("meaning").placeholder = "Enter a meaning representation...";
			document.getElementById('production_result').style.display = "none";
			document.getElementById('comprehension_result').style.display = "none";

			showLoader();

			/* Send ajax request to FCG server */
			$.ajax({
				type: "POST",
				textData: "json",
				url: comprehension_route,
				contentType: 'application/json',
				data: JSON.stringify({'utterance': utterance,
				'timeout': comprehension_timeout,
				'package': package,
				'grammar': chosenGrammar}),
				success: function (data) {
					response = JSON.parse(data);
					document.getElementById("result_utterance_comprehension").innerHTML = utterance;
					document.getElementById("result_meaning_comprehension").innerHTML = response['meaning'];
					document.getElementById("graph_comprehension").innerHTML = response['constructionalDependencies'];
					hideLoader();
					document.getElementById('comprehension_result').style.display = "block";
				},
				statusCode: {
					500: function(data) {
						alert('Timeout Exceededor other internal server error.');
						hideLoader();
					}
				},
				error: function (data) {
				},
			});
		} else {
			alert("Please enter an utterance in the textbox for Comprehension.");
		}
	} else {
		alert("Please choose a grammar first.");
	}
}

/*Production */

function produce() {
	if (chosenGrammar) {
		meaning = document.getElementById("meaning").value;
		if (meaning) {
			/* Reset utterance related fields */
			document.getElementById("utterance").value = "";
			document.getElementById("utterance").placeholder = "Enter an utterance...";
			document.getElementById('production_result').style.display = "none";
			document.getElementById('comprehension_result').style.display = "none";

			showLoader();

			$.ajax({
				type: "POST",
				textData: "json",
				url: production_route,
				contentType: 'application/json',
				data: JSON.stringify({'meaning': meaning,
															'timeout': production_timeout,
															'package': package,
															'grammar': chosenGrammar}),
				success: function (data) {
					response = JSON.parse(data);
					document.getElementById("result_meaning_production").innerHTML = meaning;
					document.getElementById("result_utterance_production").innerHTML = response['utterance'];
					document.getElementById("graph_production").innerHTML = response['constructionalDependencies'];
					hideLoader();
					document.getElementById('production_result').style.display = "block";
				},

				statusCode: {
					500: function(data) {
						alert('Timeout Exceeded or other internal server error.');
						hideLoader();
					}
				},
				error: function (data) {
				},
			});

		} else {
			alert("Please enter a meaning representation in the textbox for Production.");
		}
	} else {
		alert("Please choose a grammar first.");
	}
}


/* Showing the loader */

function showLoader() {
	document.getElementById('loading').style.display = "block";
}

function hideLoader() {
	document.getElementById('loading').style.display = "none";
}

/* Resetting the interface */

function resetInterface() {
	utterance="";
	chosenGrammar="";
	meaning = "";
	package = "";
	document.getElementById('grammarComboBox').value = "";
	document.getElementById("utterance").value = "";
	document.getElementById("utterance").placeholder = "Enter an utterance...";
	document.getElementById("meaning").value = "";
	document.getElementById("meaning").placeholder = "Enter a meaning representation...";
	document.getElementById('production_result').style.display = "none";
	document.getElementById('comprehension_result').style.display = "none";
  hideLoader();
}

/* Loading example sentences for the chosenGrammar*/


function loadUtterances() {
	var list = document.getElementById('utterances');
	while (list.firstChild) {
		list.removeChild(list.firstChild);
	}

	$.ajax({
		type: "POST",
		textData: "json",
		url: examples_route,
		contentType: 'application/json',
		data: JSON.stringify({'direction': 'comprehension',
													'package': package,
													'grammar': chosenGrammar}),
		success: function (data) {
			utterances = JSON.parse(data)['examples'];

			for (var example of utterances) {
				var option = document.createElement('option');
				option.value = example;
				list.appendChild(option);
			}
		},
		statusCode: {
			500: function(data) {
				alert('Internal server error.');
			}
		},
		error: function (data) {
		},
	});
}

function loadMeanings() {
	var list = document.getElementById('meanings');
	while (list.firstChild) {
		list.removeChild(list.firstChild);
	}

	$.ajax({
		type: "POST",
		textData: "json",
		url: examples_route,
		contentType: 'application/json',
		data: JSON.stringify({'direction': 'production',
													'package': package,
													'grammar': chosenGrammar}),
		success: function (data) {
			utterances = JSON.parse(data)['examples'];

			for (var example of utterances) {
				var option = document.createElement('option');
				option.value = example;
				list.appendChild(option);
			}
		},
		statusCode: {
			500: function(data) {
				alert('Internal server error.');
			}
		},
		error: function (data) {
		},
	});
}
