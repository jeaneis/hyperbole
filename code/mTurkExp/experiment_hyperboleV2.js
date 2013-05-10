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

function shuffledSampleArray(arrLength, sampleLength)
{
  var arr = shuffledArray(arrLength);
  var beginIndex = Math.floor(Math.random() * (arrLength-sampleLength+1));
  return arr.slice(beginIndex, beginIndex+sampleLength);
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

function clearSliderForm(oForm) {
    
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

function randomizeSharpOffset()
{
  var r = Math.floor((Math.random()*6)+1);
  if (r < 4) { return r; }
  else { return 3-r; }
}

var allConditions = 
[
[
{"sentenceID":1,"domain":"electric kettle","modifier":"an","number":"round","utterance":20,"buyer":"Alex"},
{"sentenceID":2,"domain":"electric kettle","modifier":"an","number":"round","utterance":50,"buyer":"Bob"},
{"sentenceID":3,"domain":"electric kettle","modifier":"an","number":"round","utterance":100,"buyer":"Calvin"},
{"sentenceID":4,"domain":"electric kettle","modifier":"an","number":"round","utterance":200,"buyer":"Dave"},
{"sentenceID":5,"domain":"electric kettle","modifier":"an","number":"round","utterance":500,"buyer":"Ed"},
{"sentenceID":6,"domain":"electric kettle","modifier":"an","number":"round","utterance":1000,"buyer":"Frank"},
{"sentenceID":7,"domain":"electric kettle","modifier":"an","number":"round","utterance":2000,"buyer":"George"},
{"sentenceID":8,"domain":"electric kettle","modifier":"an","number":"round","utterance":10000,"buyer":"Harry"},
{"sentenceID":9,"domain":"electric kettle","modifier":"an","number":"sharp","utterance":20,"buyer":"Ivan"},
{"sentenceID":10,"domain":"electric kettle","modifier":"an","number":"sharp","utterance":50,"buyer":"Jake"},
{"sentenceID":11,"domain":"electric kettle","modifier":"an","number":"sharp","utterance":100,"buyer":"Kevin"},
{"sentenceID":12,"domain":"electric kettle","modifier":"an","number":"sharp","utterance":200,"buyer":"Luke"},
{"sentenceID":13,"domain":"electric kettle","modifier":"an","number":"sharp","utterance":500,"buyer":"Mark"},
{"sentenceID":14,"domain":"electric kettle","modifier":"an","number":"sharp","utterance":1000,"buyer":"Ned"},
{"sentenceID":15,"domain":"electric kettle","modifier":"an","number":"sharp","utterance":2000,"buyer":"Owen"},
{"sentenceID":16,"domain":"electric kettle","modifier":"an","number":"sharp","utterance":10000,"buyer":"Paul"},
{"sentenceID":17,"domain":"coffee maker","modifier":"a","number":"round","utterance":20,"buyer":"Ryan"},
{"sentenceID":18,"domain":"coffee maker","modifier":"a","number":"round","utterance":50,"buyer":"Steve"},
{"sentenceID":19,"domain":"coffee maker","modifier":"a","number":"round","utterance":100,"buyer":"Terry"},
{"sentenceID":20,"domain":"coffee maker","modifier":"a","number":"round","utterance":200,"buyer":"Victor"},
{"sentenceID":21,"domain":"coffee maker","modifier":"a","number":"round","utterance":500,"buyer":"Albert"},
{"sentenceID":22,"domain":"coffee maker","modifier":"a","number":"round","utterance":1000,"buyer":"Barry"},
{"sentenceID":23,"domain":"coffee maker","modifier":"a","number":"round","utterance":2000,"buyer":"Cameron"},
{"sentenceID":24,"domain":"coffee maker","modifier":"a","number":"round","utterance":10000,"buyer":"Darren"},
{"sentenceID":25,"domain":"coffee maker","modifier":"a","number":"sharp","utterance":20,"buyer":"Eli"},
{"sentenceID":26,"domain":"coffee maker","modifier":"a","number":"sharp","utterance":50,"buyer":"Felix"},
{"sentenceID":27,"domain":"coffee maker","modifier":"a","number":"sharp","utterance":100,"buyer":"Gabe"},
{"sentenceID":28,"domain":"coffee maker","modifier":"a","number":"sharp","utterance":200,"buyer":"Hal"},
{"sentenceID":29,"domain":"coffee maker","modifier":"a","number":"sharp","utterance":500,"buyer":"Irvin"},
{"sentenceID":30,"domain":"coffee maker","modifier":"a","number":"sharp","utterance":1000,"buyer":"Jerry"},
{"sentenceID":31,"domain":"coffee maker","modifier":"a","number":"sharp","utterance":2000,"buyer":"Kenneth"},
{"sentenceID":32,"domain":"coffee maker","modifier":"a","number":"sharp","utterance":10000,"buyer":"Larry"},
{"sentenceID":33,"domain":"sweater","modifier":"a","number":"round","utterance":20,"buyer":"Martin"},
{"sentenceID":34,"domain":"sweater","modifier":"a","number":"round","utterance":50,"buyer":"Nelson"},
{"sentenceID":35,"domain":"sweater","modifier":"a","number":"round","utterance":100,"buyer":"Oscar"},
{"sentenceID":36,"domain":"sweater","modifier":"a","number":"round","utterance":200,"buyer":"Perry"},
{"sentenceID":37,"domain":"sweater","modifier":"a","number":"round","utterance":500,"buyer":"Ron"},
{"sentenceID":38,"domain":"sweater","modifier":"a","number":"round","utterance":1000,"buyer":"Sam"},
{"sentenceID":39,"domain":"sweater","modifier":"a","number":"round","utterance":2000,"buyer":"Ted"},
{"sentenceID":40,"domain":"sweater","modifier":"a","number":"round","utterance":10000,"buyer":"Andrew"},
{"sentenceID":41,"domain":"sweater","modifier":"a","number":"sharp","utterance":20,"buyer":"Bart"},
{"sentenceID":42,"domain":"sweater","modifier":"a","number":"sharp","utterance":50,"buyer":"Corey"},
{"sentenceID":43,"domain":"sweater","modifier":"a","number":"sharp","utterance":100,"buyer":"Don"},
{"sentenceID":44,"domain":"sweater","modifier":"a","number":"sharp","utterance":200,"buyer":"Edward"},
{"sentenceID":45,"domain":"sweater","modifier":"a","number":"sharp","utterance":500,"buyer":"Fred"},
{"sentenceID":46,"domain":"sweater","modifier":"a","number":"sharp","utterance":1000,"buyer":"Garrett"},
{"sentenceID":47,"domain":"sweater","modifier":"a","number":"sharp","utterance":2000,"buyer":"Henry"},
{"sentenceID":48,"domain":"sweater","modifier":"a","number":"sharp","utterance":10000,"buyer":"Isaac"},
{"sentenceID":49,"domain":"headphones","modifier":"some","number":"round","utterance":20,"buyer":"Jordan"},
{"sentenceID":50,"domain":"headphones","modifier":"some","number":"round","utterance":50,"buyer":"Keith"},
{"sentenceID":51,"domain":"headphones","modifier":"some","number":"round","utterance":100,"buyer":"Laurence "},
{"sentenceID":52,"domain":"headphones","modifier":"some","number":"round","utterance":200,"buyer":"Matt"},
{"sentenceID":53,"domain":"headphones","modifier":"some","number":"round","utterance":500,"buyer":"Norman"},
{"sentenceID":54,"domain":"headphones","modifier":"some","number":"round","utterance":1000,"buyer":"Oliver"},
{"sentenceID":55,"domain":"headphones","modifier":"some","number":"round","utterance":2000,"buyer":"Phillip"},
{"sentenceID":56,"domain":"headphones","modifier":"some","number":"round","utterance":10000,"buyer":"Russell"},
{"sentenceID":57,"domain":"headphones","modifier":"some","number":"sharp","utterance":20,"buyer":"Seth"},
{"sentenceID":58,"domain":"headphones","modifier":"some","number":"sharp","utterance":50,"buyer":"Tim"},
{"sentenceID":59,"domain":"headphones","modifier":"some","number":"sharp","utterance":100,"buyer":"Aaron"},
{"sentenceID":60,"domain":"headphones","modifier":"some","number":"sharp","utterance":200,"buyer":"Ben"},
{"sentenceID":61,"domain":"headphones","modifier":"some","number":"sharp","utterance":500,"buyer":"Cyrus"},
{"sentenceID":62,"domain":"headphones","modifier":"some","number":"sharp","utterance":1000,"buyer":"Daniel"},
{"sentenceID":63,"domain":"headphones","modifier":"some","number":"sharp","utterance":2000,"buyer":"Eric"},
{"sentenceID":64,"domain":"headphones","modifier":"some","number":"sharp","utterance":10000,"buyer":"Chris"},
{"sentenceID":65,"domain":"watch","modifier":"a","number":"round","utterance":20,"buyer":"Charles"},
{"sentenceID":66,"domain":"watch","modifier":"a","number":"round","utterance":50,"buyer":"Brian"},
{"sentenceID":67,"domain":"watch","modifier":"a","number":"round","utterance":100,"buyer":"Adam"},
{"sentenceID":68,"domain":"watch","modifier":"a","number":"round","utterance":200,"buyer":"Michael"},
{"sentenceID":69,"domain":"watch","modifier":"a","number":"round","utterance":500,"buyer":"James"},
{"sentenceID":70,"domain":"watch","modifier":"a","number":"round","utterance":1000,"buyer":"John"},
{"sentenceID":71,"domain":"watch","modifier":"a","number":"round","utterance":2000,"buyer":"Douglas"},
{"sentenceID":72,"domain":"watch","modifier":"a","number":"round","utterance":10000,"buyer":"Justin"},
{"sentenceID":73,"domain":"watch","modifier":"a","number":"sharp","utterance":20,"buyer":"Billy"},
{"sentenceID":74,"domain":"watch","modifier":"a","number":"sharp","utterance":50,"buyer":"Howard"},
{"sentenceID":75,"domain":"watch","modifier":"a","number":"sharp","utterance":100,"buyer":"Todd"},
{"sentenceID":76,"domain":"watch","modifier":"a","number":"sharp","utterance":200,"buyer":"Alan"},
{"sentenceID":77,"domain":"watch","modifier":"a","number":"sharp","utterance":500,"buyer":"Jeff"},
{"sentenceID":78,"domain":"watch","modifier":"a","number":"sharp","utterance":1000,"buyer":"Ray"},
{"sentenceID":79,"domain":"watch","modifier":"a","number":"sharp","utterance":2000,"buyer":"Tom"},
{"sentenceID":80,"domain":"watch","modifier":"a","number":"sharp","utterance":10000,"buyer":"Rick"},
{"sentenceID":81,"domain":"laptop","modifier":"a","number":"round","utterance":20,"buyer":"Ian"},
{"sentenceID":82,"domain":"laptop","modifier":"a","number":"round","utterance":50,"buyer":"Kirk"},
{"sentenceID":83,"domain":"laptop","modifier":"a","number":"round","utterance":100,"buyer":"Sean"},
{"sentenceID":84,"domain":"laptop","modifier":"a","number":"round","utterance":200,"buyer":"Zach"},
{"sentenceID":85,"domain":"laptop","modifier":"a","number":"round","utterance":500,"buyer":"Dean"},
{"sentenceID":86,"domain":"laptop","modifier":"a","number":"round","utterance":1000,"buyer":"Theodore"},
{"sentenceID":87,"domain":"laptop","modifier":"a","number":"round","utterance":2000,"buyer":"Rodney"},
{"sentenceID":88,"domain":"laptop","modifier":"a","number":"round","utterance":10000,"buyer":"Will"},
{"sentenceID":89,"domain":"laptop","modifier":"a","number":"sharp","utterance":20,"buyer":"Jack"},
{"sentenceID":90,"domain":"laptop","modifier":"a","number":"sharp","utterance":50,"buyer":"Roger"},
{"sentenceID":91,"domain":"laptop","modifier":"a","number":"sharp","utterance":100,"buyer":"Greg"},
{"sentenceID":92,"domain":"laptop","modifier":"a","number":"sharp","utterance":200,"buyer":"Scott"},
{"sentenceID":93,"domain":"laptop","modifier":"a","number":"sharp","utterance":500,"buyer":"Joseph"},
{"sentenceID":94,"domain":"laptop","modifier":"a","number":"sharp","utterance":1000,"buyer":"Richard"},
{"sentenceID":95,"domain":"laptop","modifier":"a","number":"sharp","utterance":2000,"buyer":"Carl"},
{"sentenceID":96,"domain":"laptop","modifier":"a","number":"sharp","utterance":10000,"buyer":"Leon"},
]
];


var numConditions = allConditions.length;
var chooseCondition = random(0, numConditions-1);
var allTrialOrders = allConditions[chooseCondition];
var numTrials = allTrialOrders.length / 3;
var shuffledOrder = shuffledSampleArray(allTrialOrders.length, numTrials);
var currentTrialNum = 0;
var trial;
var numComplete = 0;
var buyer;

showSlide("instructions");
$("#trial-num").html(numComplete);
$("#total-num").html(numTrials);


var experiment = {
	condition: chooseCondition + 1,
	sentenceIDs: new Array(numTrials),
	utteredPrices: new Array(numTrials),
  inferredPrices: new Array(numTrials),
  affects: new Array(numTrials),
  orders: new Array(numTrials),
  domains: new Array(numTrials),
  buyers: new Array(numTrials),
  numberTypes: new Array(numTrials),
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
    var gen = getRadioCheckedValue(2, "genderButton");
    var ag = document.age.ageRange.value;
    var lan = document.language.nativeLanguage.value;
    var comm = document.comments.input.value;
    var incomeVal = document.income.incomeRange.value;
    experiment.gender = gen;
    experiment.age = ag;
    experiment.nativeLanguage = lan;
    experiment.comments = comm;
    experiment.income = incomeVal;
    clearForm(document.forms[2]);
    clearForm(document.forms[3]);
    clearForm(document.forms[4]);
    clearForm(document.forms[5]);
    clearForm(document.forms[6]);    
    showSlide("finished");
    setTimeout(function() {turk.submit(experiment) }, 1500);
  },
  next: function() {
    if (numComplete > 0) {
      var price = parseFloat(document.price.score.value) + parseFloat(document.price.score1.value) / 100.00;
    	var likely = parseInt(document.getElementById("hiddenSliderValue0").value) / 40.00;
      experiment.inferredPrices[currentTrialNum] = price;
      experiment.affects[currentTrialNum] = likely;
      experiment.orders[currentTrialNum] = numComplete;
      experiment.sentenceIDs[currentTrialNum] = trial.sentenceID;
      experiment.domains[currentTrialNum] = trial.domain;
      experiment.buyers[currentTrialNum] = trial.buyer;
      experiment.utteredPrices[currentTrialNum] = document.getElementById("cost").innerHTML;
      experiment.numberTypes[currentTrialNum] = trial.number;
        	
      clearForm(document.forms[0]);
      clearForm(document.forms[1]);
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
    	currentTrialNum = numComplete;
    	trial = allTrialOrders[shuffledOrder[numComplete]];
//     	currentTrialNum = shuffledOrder[numComplete];
//     	trial = allTrialOrders[currentTrialNum];
    	buyer = trial.buyer;
      showSlide("stage");
      $("#buyer1").html(buyer);
      $("#buyer2").html(buyer);
      $("#buyer3").html(buyer);
      $("#domain1").html(trial.domain);
      $("#domain2").html(trial.domain);
      $("#domain3").html(trial.domain);
      $("#modifier").html(trial.modifier);
      if ( trial.number == "round" )
      { $("#cost").html(trial.utterance); }
      else
      { $("#cost").html(trial.utterance + randomizeSharpOffset()); }
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

