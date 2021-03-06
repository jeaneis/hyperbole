function showSlide(id) {
  $(".slide").hide();
  $("#"+id).show();
}

function random(a,b) {
  if (typeof b == "undefined") {
    a = a || 2;
    return Math.floor(Math.random()*a);
  } else {
    return Math.floor(Math.random()*(b-a+1)) + a;
  }
}

Array.prototype.random = function() {
  return this[random(this.length)];
}

function setQuestion(array) {
    var i = random(0, array.length - 1);
    var q = array[i];
    return q;
}

function shuffledArray(arrLength)
{
  var j, tmp;
  var arr = new Array(arrLength);
  for (i = 0; i < arrLength; i++)
  {
    arr[i] = i;
  }
  for (i = 0; i < arrLength-1; i++)
  {
    j = Math.floor((Math.random() * (arrLength - 1 - i)) + 0.99) + i;
    tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }
  return arr;
}

function getRadioCheckedValue(formNum, radio_name)
{
   var oRadio = document.forms[formNum].elements[radio_name];
   for(var i = 0; i < oRadio.length; i++)
   {
      if(oRadio[i].checked)
      {
         return oRadio[i].value;
      }
   }
   return '';
}

function clearForm(oForm) {
    
  var elements = oForm.elements; 
    
  oForm.reset();

  for(i=0; i<elements.length; i++) {
      
	field_type = elements[i].type.toLowerCase();
	
	switch(field_type) {
	
		case "text": 
		case "password": 
		case "textarea":
	        case "hidden":	
			
			elements[i].value = ""; 
			break;
        
		case "radio":
		case "checkbox":
  			if (elements[i].checked) {
   				elements[i].checked = false; 
			}
			break;

		case "select-one":
		case "select-multi":
            		elements[i].selectedIndex = -1;
			break;

		default: 
			break;
	}
    }
}



var allConditions = 
[
[
{"sentenceID":1, "domain":"sweater", "unit":"dollars", "verb":"bought", "adj":"much", "verb2":"cost", "modifier":"a"},
{"sentenceID":2, "domain":"headphones", "unit":"dollars", "verb":"bought", "adj":"much", "verb2":"cost", "modifier":"some"},
{"sentenceID":3, "domain":"laptop", "unit":"dollars", "verb":"bought", "adj":"much", "verb2":"cost", "modifier":"a"},
{"sentenceID":4, "domain":"coffee maker", "unit":"dollars", "verb":"bought", "adj":"much", "verb2":"cost", "modifier":"a"},
{"sentenceID":5, "domain":"watch", "unit":"dollars", "verb":"bought", "adj":"much", "verb2":"cost", "modifier":"a"},
{"sentenceID":6, "domain":"electric kettle", "unit":"dollars", "verb":"bought", "adj":"much", "verb2":"cost", "modifier":"an"},
]
];

var speakers = ["Alex", "Beverly", "Chris", "Devin", "Ellis", "Felix"];
var buyers = ["Jessie", "Kasey", "Logan", "Morgan", "Nell", "Leslie"];

var numConditions = allConditions.length;
var chooseCondition = random(0, numConditions-1);
var allTrialOrders = allConditions[chooseCondition];
var numTrials = allTrialOrders.length;
var shuffledOrder = shuffledArray(numTrials);
var currentTrialNum = 0;
var trial;
var numComplete = 0;
var speaker;
var buyer;

showSlide("instructions");
$("#trial-num").html(numComplete);
$("#total-num").html(numTrials);


var experiment = {
	condition: chooseCondition + 1,
	sentenceIDs: new Array(numTrials),
    results: new Array(numTrials),
    orders: new Array(numTrials),
    modifiers: new Array(numTrials),
    domains: new Array(numTrials),
    speakers: new Array(numTrials),
    buyers: new Array(numTrials),
    gender: "",
    age:"",
    income:"",
    nativeLanguage:"",
    comments:"",
    description: function() {
    	
    	showSlide("description");
    	$("#tot-num").html(numTrials);
    	
    },
    end: function() {
        var gen = getRadioCheckedValue(1, "genderButton");
        var ag = document.age.ageRange.value;
        var lan = document.language.nativeLanguage.value;
        var comm = document.comments.input.value;
        var incomeVal = document.income.incomeRange.value;
        experiment.gender = gen;
        experiment.age = ag;
        experiment.nativeLanguage = lan;
        experiment.comments = comm;
        experiment.income = incomeVal;
        clearForm(document.forms[1]);
        clearForm(document.forms[2]);
        clearForm(document.forms[3]);
        clearForm(document.forms[4]);
        clearForm(document.forms[5]);
        
        showSlide("finished");
        setTimeout(function() {turk.submit(experiment) }, 1500);
    },
    next: function() {
    	if (numComplete > 0) {
    		
    		var price = parseFloat(document.price.score.value) + parseFloat(document.price.score1.value) / 100.00;
        	experiment.results[currentTrialNum] = price;
        	experiment.orders[currentTrialNum] = numComplete;
        	
        	experiment.sentenceIDs[currentTrialNum] = trial.sentenceID;
        	experiment.domains[currentTrialNum] = trial.domain;
        	experiment.modifiers[currentTrialNum] = trial.modifier;
        	
        	experiment.speakers[currentTrialNum] = speaker;
        	experiment.buyers[currentTrialNum] = buyer;
        	
        	clearForm(document.forms[0]);
        
        }
    	if (numComplete >= numTrials) {
    		$('.bar').css('width', (200.0 * numComplete/numTrials) + 'px');
    		$("#trial-num").html(numComplete);
    		$("#total-num").html(numTrials);
    		showSlide("askInfo");
    	} else {
    		$('.bar').css('width', (200.0 * numComplete/numTrials) + 'px');
    		$("#trial-num").html(numComplete);
    		$("#total-num").html(numTrials);
    		currentTrialNum = shuffledOrder[numComplete];
    		trial = allTrialOrders[currentTrialNum];
    		speaker = speakers[currentTrialNum];
    		buyer = buyers[currentTrialNum];
        	showSlide("stage");
        	$("#buyer").html(buyer);
        	$("#speaker").html(speaker);
        	$("#domain1").html(trial.domain);
        	$("#domain2").html(trial.domain);
        	$("#modifier").html(trial.modifier);
        	$("#unit").html(trial.unit);
        	$("#adj").html(trial.adj);
        	$("#verb").html(trial.verb);
        	$("#verb2").html(trial.verb2);
        	numComplete++;
        }
    }
}


