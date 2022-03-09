/* Change this to the correct url for the fcg-server*/
var url = "https://www.fcg-net.org/fcg-interactive/fcg-request"; 
/*var url = "http://localhost:8920";*/

var chosenGrammar = "";
var package = "fcg";
var monitor = "trace-fcg-interactive-no-car";

var utterance = "";
var meaning = "";

function grammarChosen(value) {
	switch (value) {
		case "english":
			chosenGrammar = "*fcg-constructions*";
			package = "english-grammar";
			break;
		case "portuguese":
			chosenGrammar = "*portuguese-corpus-grammar*";
			package = "fcg";
			break;
		case "russian":
			package = "fcg";
			chosenGrammar = "*russian-motion-grammar*";
			break;

	}
	resetResult();
	showGrammar();
	if (browser != "safari") {
		loadSentences();
		loadMeanings();
	}
	loadReferenceField();
}

function showGrammar() {
	var params = {
		"handler-method": "fcg-get-cxn-inventory",
		"cxn-inventory": chosenGrammar
	};

	var httpRequest_sg = new XMLHttpRequest();
	httpRequest_sg.open("POST", url, true);
	httpRequest_sg.setRequestHeader('Content-type', 'application/json');
	httpRequest_sg.onreadystatechange = function() {
		if (httpRequest_sg.readyState == XMLHttpRequest.DONE) {
			link = JSON.parse(httpRequest_sg.responseText);
			document.getElementById("cxnInv").style.visibility = "visible";
			window.open(link.fcgResponse, "cxnInv");
		}
	}
	httpRequest_sg.send(JSON.stringify(params));
}

function loadReferenceField() {

	var params = {
		"handler-method": "fcg-get-reference-text",
		"cxn-inventory": chosenGrammar
	};

	var httpRequest_r = new XMLHttpRequest();
	httpRequest_r.open("POST", url, true);
	httpRequest_r.setRequestHeader('Content-type', 'application/json', 'Connection', 'Close');
	httpRequest_r.onreadystatechange = function() {
		if (httpRequest_r.readyState == XMLHttpRequest.DONE) {
			var reference = JSON.parse(httpRequest_r.responseText);
			reference = reference.fcgResponse
			document.getElementById("reference_field").style.visibility = "visible";
			document.getElementById("reference_field").innerHTML = reference;
		}
	}
	httpRequest_r.send(JSON.stringify(params));
};

function loadSentences() {
	var utterances = "";
	var list = document.getElementById('utterances');
	while (list.firstChild) {
		list.removeChild(list.firstChild);
	}

	var params = {
		"handler-method": "fcg-get-example-sentences",
		"cxn-inventory": chosenGrammar
	};

	var httpRequest_s = new XMLHttpRequest();
	httpRequest_s.open("POST", url, true);
	httpRequest_s.setRequestHeader('Content-type', 'application/json', );
	httpRequest_s.onreadystatechange = function() {
		if (httpRequest_s.readyState == XMLHttpRequest.DONE) {
			var utterances = JSON.parse(httpRequest_s.responseText);
			utterances = utterances.fcgResponse.split(",");
			utterances.forEach(function(item) {
				var option = document.createElement('option');
				option.value = item;
				list.appendChild(option);
			});
		}
	}
	httpRequest_s.send(JSON.stringify(params));
}

function loadMeanings() {
	var meanings = "";
	var list = document.getElementById('meanings');
	while (list.firstChild) {
		list.removeChild(list.firstChild);
	}

	var params = {
		"handler-method": "fcg-get-example-meanings",
		"cxn-inventory": chosenGrammar
	};

	var httpRequest_m = new XMLHttpRequest();
	httpRequest_m.open("POST", url, true);
	httpRequest_m.setRequestHeader('Content-type', 'application/json', );
	httpRequest_m.onreadystatechange = function() {
		if (httpRequest_m.readyState == XMLHttpRequest.DONE) {
			var utterances = JSON.parse(httpRequest_m.responseText);
			utterances = utterances.fcgResponse.split(",");
			utterances.forEach(function(item) {
				var option = document.createElement('option');
				option.value = item;
				list.appendChild(option);
			});
		}
	}
	httpRequest_m.send(JSON.stringify(params));
}

function comprehend() {
	if (chosenGrammar) {
		utterance = document.getElementById("utterance").value;
		if (utterance) {
			document.getElementById("meaning").value = "";
			document.getElementById("meaning").placeholder = "Your meaning (click for examples)...";
			sendHttpRequestFcg("fcg-comprehend", "utterance", utterance);
		} else {
			alert("Please enter an utterance in the textbox for Comprehension.");
		}
	} else {
		alert("Please choose a grammar first.");
	}
}

function comprehendAndFormulate() {
	utterance = document.getElementById("utterance").value;
	if (chosenGrammar) {
		if (utterance) {
			document.getElementById("meaning").value = "";
			document.getElementById("meaning").placeholder = "Your meaning (click for examples)...";
			sendHttpRequestFcg("fcg-comprehend-and-formulate", "utterance", utterance);
		} else {
			alert("Please enter an utterance in the textbox for Comprehension.");
		}
	} else {
		alert("Please choose a grammar first.");
	}
}

function formulate() {
	meaning = document.getElementById("meaning").value;
	if (checkMeaningFormat(meaning) == false) {
		alert('The meaning you have entered does not fit the format (symbol symbol) (symbol symbol symbol), ...');
	} else {
		if (chosenGrammar) {
			if (meaning) {
				document.getElementById("utterance").value = "";
				document.getElementById("utterance").placeholder = "Your utterance (click for examples)...";
				sendHttpRequestFcg("fcg-formulate", "meaning", meaning);
			} else {
				alert("Please enter an meaning in the textbox for Formulation.");
			}
		} else {
			alert("Please choose a grammar first.");
		}
	}
}

function formulateAndComprehend() {
	meaning = document.getElementById("meaning").value;
	if (checkMeaningFormat(meaning) == false) {
		alert('The meaning you have entered does not fit the format (symbol symbol) (symbol symbol symbol), ...');
	} else {
		if (chosenGrammar) {
			if (meaning) {
				document.getElementById("utterance").value = "";
				document.getElementById("utterance").placeholder = "Your utterance (click for examples)...";
				sendHttpRequestFcg("fcg-formulate-and-comprehend", "meaning", meaning);
			} else {
				alert("Please enter an meaning in the textbox for Formulation.");
			}
		} else {
			alert("Please choose a grammar first.");
		}
	}
}

function sendHttpRequestFcg(method, inputType, input) {
	var httpRequest = new XMLHttpRequest();
	var params = {
		"handler-method": method,
		"package": package,
		"cxn-inventory": chosenGrammar,
		"visualisation": "link-only",
		"monitor": monitor
	};
	params[inputType] = input;

	httpRequest.open("POST", url, true);
	httpRequest.setRequestHeader('Content-type', 'application/json');
	var iframe = document.getElementById("fcgResult");
	iframe.style.visibility = "visible";
	window.open("loading.html", "fcgResult");
	httpRequest.onreadystatechange = function() {
		if (httpRequest.readyState == XMLHttpRequest.DONE) {
			var response = JSON.parse(httpRequest.responseText);
			window.open(response.fcgResponse, "fcgResult");
		}
	}
	httpRequest.send(JSON.stringify(params));
}

function resetInterface() {
	resetResult();
	document.getElementById('grammarComboBox').value = "";
	chosenGrammar = "";
	package = "fcg";
}

function resetResult() {

	document.getElementById("reference_field").style.visibility = "hidden";
	document.getElementById("fcgResult").style.visibility = "hidden";
	document.getElementById("cxnInv").style.visibility = "hidden";


	document.getElementById('visualize_car').checked = false;
	monitor = 'trace-fcg-interactive-no-car';

	var ut = document.getElementById('utterances');
	while (ut.firstChild) {
		ut.removeChild(ut.firstChild);
	}

	var mean = document.getElementById('meanings');
	while (mean.firstChild) {
		mean.removeChild(mean.firstChild);
	}

	utterance = "";
	meaning = "";
	document.getElementById("utterance").value = "";
	document.getElementById("utterance").placeholder = "Your utterance (click for examples)...";

	document.getElementById("meaning").value = "";
	document.getElementById("meaning").placeholder = "Your meaning (click for examples)...";

	window.open("about:blank", "fcgResult")
}

document.getElementById('visualize_car').onclick = function() {
	if (this.checked) {
		monitor = 'trace-fcg-interactive';
	} else {
		monitor = 'trace-fcg-interactive-no-car';
	}
}

/* #######################################################################################
	Code for checking the browser, etc.
####################################################################################### */

var browser = check_browser();

function check_browser() {
	var userAgent = navigator.userAgent.toLowerCase();

	if (userAgent.indexOf('firefox') > 0) {
		return 'firefox';
	} else if (userAgent.indexOf('edge') > 0) {
		return 'edge';
	} else if (userAgent.indexOf('trident') > 0) {
		return 'trident';
	} else if (userAgent.indexOf('chrome') > 0) {
		return 'chrome';
	} else if (userAgent.indexOf('safari') > 0) {
		return 'safari';
	} else {
		return "unknown_browser";
	}
}

if (browser == "safari") {
	showSafariError();
}

if (browser == "trident") {
	showTridentError();
}

if (browser == "edge") {
	showTridentError();
}

function showSafariError() {
	alert('FCG Interactive does not always work well in Safari. \n\nFor optimal results, we recommend to use Mozilla Firefox, although Google Chrome also seems to work fine.');
};

function showTridentError() {
	alert('FCG Interactive does not work well in Internet Explorer of Microsoft Edge. \n\nFor optimal results, we recommend to use Mozilla Firefox, although Google Chrome also seems to work fine.');
};

/* #######################################################################################
	Code for checking the format of a meaning to formulate
####################################################################################### */


function checkMeaningFormat(str) {
	/* Returns true if the string has a valid meaning format.
	   Ignoring optional outer parenthesises, checks whether 
	   the inner S-expr elements are valid and unnested
	   S-exprs.
	*/
	var strarray = splitSExpr(str);
	if (strarray === false) {
		return false;
	}
	if (strarray.length == 1) {
		if (checkFactFormat(strarray[0])) {
			return true;
		}
		str = strarray[0];
		if (str.length > 2) {
			if (!(str[0] == '(' && str[str.length - 1] == ')')) {
				return false;
			}
		} else {
			return false;
		}
		str = str.substr(1, str.length - 2);
		strarray = splitSExpr(str);
	}
	for (var i = 0; i < strarray.length; i++) {
		if (!checkFactFormat(strarray[i])) {
			return false;
		}
	}
	return true;
}

function splitSExpr(str) {
	var strarray = [];
	str = eatWhite(str);
	while (str.length > 0) {
		var end = 0;
		if (str[0] == '(') {
			end += 1;
			if (end == str.length) {
				return false;
			}
			nesting = 0;
			while (!(str[end] == ')' && nesting === 0)) {
				if (str[end] == '(') {
					nesting += 1;
				}
				if (str[end] == ')') {
					nesting -= 1;
				}
				end += 1;
				if (end == str.length) {
					return false;
				}
			}
			end += 1;
		} else {
			while (str[end] != ' ' && end < str.length) {
				// TODO return false if bad char in name format
				end += 1;
			}
		}
		strarray.push(str.substr(0, end));
		str = eatWhite(str.substr(end));
	}
	return strarray;
}

function eatWhite(str) {
	/* Returns the string without a possible prefix of spaces.
	 */
	var i = 0;
	if (i == str.length) {
		return '';
	}
	while (str[i] == ' ') {
		i += 1;
		if (i == str.length) {
			return '';
		}
	}
	return str.substr(i);
}

function checkFactFormat(str) {
	/* Returns true if the string has a valid fact format.
	   A fact must be enclosed by parenthesises, without 
	   inner parenthesises (more conditions may be added).
	   Something must be inside.
	*/
	var something = false;
	if (str[0] != '(') {
		return false;
	}
	if (str[str.length - 1] != ')') {
		return false;
	}
	for (var i = 1; i < str.length - 1; i++) {
		if (str[i] == '(') {
			return false;
		}
		if (str[i] == ')') {
			return false;
		}
		if (str[i] != ' ') {
			something = true;
		}
	}
	return something;
}
