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

  $("#slider0").slider("value", 20);
  $("#slider0").css({"background":"#FFFFFF"});
  $("#slider0 .ui-slider-handle").css({
      "background":"#FAFAFA",
      "border-color": "#CCCCCC" });
  document.getElementById("slider0").style.background = "";

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
{"condition":1,"modifier":"an","domain":"electric kettle","price":9.99,"buyer":"Alex"},
{"condition":1,"modifier":"an","domain":"electric kettle","price":17.99,"buyer":"Chris"},
{"condition":1,"modifier":"an","domain":"electric kettle","price":25.99,"buyer":"Ellis"},
{"condition":1,"modifier":"an","domain":"electric kettle","price":33.99,"buyer":"George"},
{"condition":1,"modifier":"an","domain":"electric kettle","price":41.99,"buyer":"Iris"},
{"condition":1,"modifier":"an","domain":"electric kettle","price":49.99,"buyer":"Kasey"},
{"condition":1,"modifier":"an","domain":"electric kettle","price":57.99,"buyer":"Morgan"},
{"condition":1,"modifier":"an","domain":"electric kettle","price":65.99,"buyer":"Owen"},
{"condition":1,"modifier":"an","domain":"electric kettle","price":73.99,"buyer":"Randy"},
{"condition":1,"modifier":"an","domain":"electric kettle","price":81.99,"buyer":"Terrance"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":8.5,"buyer":"Uri"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":23.39,"buyer":"Steph"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":38.29,"buyer":"Percy"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":53.18,"buyer":"Nell"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":68.07,"buyer":"Leslie"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":82.97,"buyer":"Jessie"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":97.86,"buyer":"Heather"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":112.76,"buyer":"Felix"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":127.65,"buyer":"Devin"},
{"condition":1,"modifier":"a","domain":"coffee maker","price":142.54,"buyer":"Beverly"},
{"condition":1,"modifier":"some","domain":"headphones","price":3.99,"buyer":"Alex"},
{"condition":1,"modifier":"some","domain":"headphones","price":35.15,"buyer":"Chris"},
{"condition":1,"modifier":"some","domain":"headphones","price":66.31,"buyer":"Ellis"},
{"condition":1,"modifier":"some","domain":"headphones","price":97.47,"buyer":"George"},
{"condition":1,"modifier":"some","domain":"headphones","price":128.63,"buyer":"Iris"},
{"condition":1,"modifier":"some","domain":"headphones","price":159.78,"buyer":"Kasey"},
{"condition":1,"modifier":"some","domain":"headphones","price":190.94,"buyer":"Morgan"},
{"condition":1,"modifier":"some","domain":"headphones","price":222.1,"buyer":"Owen"},
{"condition":1,"modifier":"some","domain":"headphones","price":253.26,"buyer":"Randy"},
{"condition":1,"modifier":"some","domain":"headphones","price":284.42,"buyer":"Terrance"},
{"condition":1,"modifier":"a","domain":"sweater","price":10,"buyer":"Uri"},
{"condition":1,"modifier":"a","domain":"sweater","price":23.16,"buyer":"Steph"},
{"condition":1,"modifier":"a","domain":"sweater","price":36.32,"buyer":"Percy"},
{"condition":1,"modifier":"a","domain":"sweater","price":49.47,"buyer":"Nell"},
{"condition":1,"modifier":"a","domain":"sweater","price":62.63,"buyer":"Leslie"},
{"condition":1,"modifier":"a","domain":"sweater","price":75.79,"buyer":"Jessie"},
{"condition":1,"modifier":"a","domain":"sweater","price":88.95,"buyer":"Heather"},
{"condition":1,"modifier":"a","domain":"sweater","price":102.11,"buyer":"Felix"},
{"condition":1,"modifier":"a","domain":"sweater","price":115.26,"buyer":"Devin"},
{"condition":1,"modifier":"a","domain":"sweater","price":128.42,"buyer":"Beverly"},
{"condition":1,"modifier":"a","domain":"watch","price":4.99,"buyer":"Alex"},
{"condition":1,"modifier":"a","domain":"watch","price":88.68,"buyer":"Chris"},
{"condition":1,"modifier":"a","domain":"watch","price":172.36,"buyer":"Ellis"},
{"condition":1,"modifier":"a","domain":"watch","price":256.05,"buyer":"George"},
{"condition":1,"modifier":"a","domain":"watch","price":339.73,"buyer":"Iris"},
{"condition":1,"modifier":"a","domain":"watch","price":423.42,"buyer":"Kasey"},
{"condition":1,"modifier":"a","domain":"watch","price":507.1,"buyer":"Morgan"},
{"condition":1,"modifier":"a","domain":"watch","price":590.79,"buyer":"Owen"},
{"condition":1,"modifier":"a","domain":"watch","price":674.47,"buyer":"Randy"},
{"condition":1,"modifier":"a","domain":"watch","price":758.16,"buyer":"Terrance"},
{"condition":1,"modifier":"a","domain":"laptop","price":237.47,"buyer":"Uri"},
{"condition":1,"modifier":"a","domain":"laptop","price":391.42,"buyer":"Steph"},
{"condition":1,"modifier":"a","domain":"laptop","price":545.37,"buyer":"Percy"},
{"condition":1,"modifier":"a","domain":"laptop","price":699.32,"buyer":"Nell"},
{"condition":1,"modifier":"a","domain":"laptop","price":853.27,"buyer":"Leslie"},
{"condition":1,"modifier":"a","domain":"laptop","price":1007.22,"buyer":"Jessie"},
{"condition":1,"modifier":"a","domain":"laptop","price":1161.17,"buyer":"Heather"},
{"condition":1,"modifier":"a","domain":"laptop","price":1315.12,"buyer":"Felix"},
{"condition":1,"modifier":"a","domain":"laptop","price":1469.07,"buyer":"Devin"},
{"condition":1,"modifier":"a","domain":"laptop","price":1623.02,"buyer":"Beverly"},
],
[
{"condition":2,"modifier":"an","domain":"electric kettle","price":13.99,"buyer":"Beverly"},
{"condition":2,"modifier":"an","domain":"electric kettle","price":21.99,"buyer":"Devin"},
{"condition":2,"modifier":"an","domain":"electric kettle","price":29.99,"buyer":"Felix"},
{"condition":2,"modifier":"an","domain":"electric kettle","price":37.99,"buyer":"Heather"},
{"condition":2,"modifier":"an","domain":"electric kettle","price":45.99,"buyer":"Jessie"},
{"condition":2,"modifier":"an","domain":"electric kettle","price":53.99,"buyer":"Leslie"},
{"condition":2,"modifier":"an","domain":"electric kettle","price":61.99,"buyer":"Nell"},
{"condition":2,"modifier":"an","domain":"electric kettle","price":69.99,"buyer":"Percy"},
{"condition":2,"modifier":"an","domain":"electric kettle","price":77.99,"buyer":"Steph"},
{"condition":2,"modifier":"an","domain":"electric kettle","price":85.99,"buyer":"Uri"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":15.95,"buyer":"Terrance"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":30.84,"buyer":"Randy"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":45.73,"buyer":"Owen"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":60.63,"buyer":"Morgan"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":75.52,"buyer":"Kasey"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":90.42,"buyer":"Iris"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":105.31,"buyer":"George"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":120.2,"buyer":"Ellis"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":135.1,"buyer":"Chris"},
{"condition":2,"modifier":"a","domain":"coffee maker","price":149.99,"buyer":"Alex"},
{"condition":2,"modifier":"some","domain":"headphones","price":19.57,"buyer":"Beverly"},
{"condition":2,"modifier":"some","domain":"headphones","price":50.73,"buyer":"Devin"},
{"condition":2,"modifier":"some","domain":"headphones","price":81.89,"buyer":"Felix"},
{"condition":2,"modifier":"some","domain":"headphones","price":113.05,"buyer":"Heather"},
{"condition":2,"modifier":"some","domain":"headphones","price":144.21,"buyer":"Jessie"},
{"condition":2,"modifier":"some","domain":"headphones","price":175.36,"buyer":"Leslie"},
{"condition":2,"modifier":"some","domain":"headphones","price":206.52,"buyer":"Nell"},
{"condition":2,"modifier":"some","domain":"headphones","price":237.68,"buyer":"Percy"},
{"condition":2,"modifier":"some","domain":"headphones","price":268.84,"buyer":"Steph"},
{"condition":2,"modifier":"some","domain":"headphones","price":300,"buyer":"Uri"},
{"condition":2,"modifier":"a","domain":"sweater","price":16.58,"buyer":"Terrance"},
{"condition":2,"modifier":"a","domain":"sweater","price":29.74,"buyer":"Randy"},
{"condition":2,"modifier":"a","domain":"sweater","price":42.89,"buyer":"Owen"},
{"condition":2,"modifier":"a","domain":"sweater","price":56.05,"buyer":"Morgan"},
{"condition":2,"modifier":"a","domain":"sweater","price":69.21,"buyer":"Kasey"},
{"condition":2,"modifier":"a","domain":"sweater","price":82.37,"buyer":"Iris"},
{"condition":2,"modifier":"a","domain":"sweater","price":95.53,"buyer":"George"},
{"condition":2,"modifier":"a","domain":"sweater","price":108.68,"buyer":"Ellis"},
{"condition":2,"modifier":"a","domain":"sweater","price":121.84,"buyer":"Chris"},
{"condition":2,"modifier":"a","domain":"sweater","price":135,"buyer":"Alex"},
{"condition":2,"modifier":"a","domain":"watch","price":46.83,"buyer":"Beverly"},
{"condition":2,"modifier":"a","domain":"watch","price":130.52,"buyer":"Devin"},
{"condition":2,"modifier":"a","domain":"watch","price":214.2,"buyer":"Felix"},
{"condition":2,"modifier":"a","domain":"watch","price":297.89,"buyer":"Heather"},
{"condition":2,"modifier":"a","domain":"watch","price":381.57,"buyer":"Jessie"},
{"condition":2,"modifier":"a","domain":"watch","price":465.26,"buyer":"Leslie"},
{"condition":2,"modifier":"a","domain":"watch","price":548.94,"buyer":"Nell"},
{"condition":2,"modifier":"a","domain":"watch","price":632.63,"buyer":"Percy"},
{"condition":2,"modifier":"a","domain":"watch","price":716.31,"buyer":"Steph"},
{"condition":2,"modifier":"a","domain":"watch","price":800,"buyer":"Uri"},
{"condition":2,"modifier":"a","domain":"laptop","price":314.44,"buyer":"Terrance"},
{"condition":2,"modifier":"a","domain":"laptop","price":468.39,"buyer":"Randy"},
{"condition":2,"modifier":"a","domain":"laptop","price":622.34,"buyer":"Owen"},
{"condition":2,"modifier":"a","domain":"laptop","price":776.29,"buyer":"Morgan"},
{"condition":2,"modifier":"a","domain":"laptop","price":930.24,"buyer":"Kasey"},
{"condition":2,"modifier":"a","domain":"laptop","price":1084.19,"buyer":"Iris"},
{"condition":2,"modifier":"a","domain":"laptop","price":1238.14,"buyer":"George"},
{"condition":2,"modifier":"a","domain":"laptop","price":1392.09,"buyer":"Ellis"},
{"condition":2,"modifier":"a","domain":"laptop","price":1546.04,"buyer":"Chris"},
{"condition":2,"modifier":"a","domain":"laptop","price":1699.99,"buyer":"Alex"},
]
];

var numConditions = allConditions.length;
var chooseCondition = random(0, numConditions-1);
var allTrialOrders = allConditions[chooseCondition];
var numTrials = allTrialOrders.length;
var shuffledOrder = shuffledArray(numTrials);
var currentTrialNum = 0;
var trial;
var numComplete = 0;

showSlide("instructions");
$("#trial-num").html(numComplete);
$("#total-num").html(numTrials);


var experiment = {
	condition: chooseCondition + 1,
	prices: new Array(numTrials),
    results: new Array(numTrials),
    orders: new Array(numTrials),
    domains: new Array(numTrials),
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
    		var likely = document.getElementById("hiddenSliderValue0").value;
//     		var likely = parseInt(getRadioCheckedValue(0, "affect"));
        	experiment.results[currentTrialNum] = likely;
        	experiment.orders[currentTrialNum] = numComplete;
        	
        	experiment.domains[currentTrialNum] = trial.domain;
        	experiment.prices[currentTrialNum] = trial.price;
        	
        	//experiment.speakers[currentTrialNum] = speaker;
        	experiment.buyers[currentTrialNum] = trial.buyer;
        	
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
        	showSlide("stage");
        	$("#buyer1").html(trial.buyer);
        	$("#buyer2").html(trial.buyer);
        	$("#domain1").html(trial.domain);
        	$("#modifier").html(trial.modifier);
        	$("#price").html(trial.price);
        	numComplete++;
        }
    }
}

// scripts for sliders
$("#slider0").slider({
               animate: true,
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider0 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue0').attr('value', ui.value);
                   $("#slider0").css({"background":"#99D6EB"});
                   $("#slider0 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
//                    displaySubmitButton();
               }});
