var NUM_SLIDERS = 7;


// Convert numbers to words
// copyright 25th July 2006, by Stephen Chapman http://javascript.about.com
// permission to use this Javascript on your web page is granted
// provided that all of the code (including this copyright notice) is
// used exactly as shown (you can change the numbering system if you wish)

// American Numbering System
var th = ['','thousand','million', 'billion','trillion'];
// uncomment this line for English Number System
// var th = ['','thousand','million', 'milliard','billion'];

var dg = ['zero','one','two','three','four', 'five','six','seven','eight','nine']; 
var tn = ['ten','eleven','twelve','thirteen', 'fourteen','fifteen','sixteen', 'seventeen','eighteen','nineteen']; 
var tw = ['twenty','thirty','forty','fifty', 'sixty','seventy','eighty','ninety']; 
function toWords(s){
	s = s.toString(); 
	s = s.replace(/[\, ]/g,''); 
	if (s != parseFloat(s)) 
		return 'not a number'; 
	var x = s.indexOf('.'); 
	if (x == -1) x = s.length; 
	if (x > 15) return 'too big'; 
	var n = s.split(''); var str = ''; 
	var sk = 0; 
	for (var i=0; i < x; i++) {
		if ((x-i)%3==2) {
			if (n[i] == '1') {
				str += tn[Number(n[i+1])] + ' '; 
				i++; 
				sk=1;
			} else if (n[i]!=0) {
				str += tw[n[i]-2] + ' ';
				sk=1;
			}
		} else if (n[i]!=0) {
			str += dg[n[i]] +' ';
			if ((x-i)%3==0) str += 'hundred ';
			sk=1;
		}
		if ((x-i)%3==1) {
			if (sk) str += th[(x-i-1)/3] + ' ';
			sk=0;
		}
	}
	if (x != s.length) {var y = s.length; str += 'point '; 
	for (var i=x+1; i<y; i++) str += dg[n[i]] +' ';}
	str = str.replace(/\s+/g, ' ');
	/*
	var words = str.split(' ');
	if (words.length > 3) {
		var words1 = words.slice(0, -2);
		str = words1.join(' ') + ' and ' + words[words.length - 2];	
	}
	*/
	return str;
}

function numberWithCommas(x) {
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

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
  var sliderVar = "";
  for(var i=0; i<NUM_SLIDERS; i++)
  {
    sliderVar = "#slider" + i;
    $(sliderVar).slider("value", 20);
    $(sliderVar).css({"background":"#FFFFFF"});
    $(sliderVar + " .ui-slider-handle").css({
        "background":"#FAFAFA",
        "border-color": "#CCCCCC" });
    sliderVar = "slider" + i;
    document.getElementById(sliderVar).style.background = "";
  }
  
  var elements = oForm.elements; 
  
  oForm.reset();

  for(var i=0; i<elements.length; i++) {
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

function randomizeSharpOffset()
{
  
  var r = Math.floor((Math.random()*6)+1);
  if (r < 4) { return r; }
  else { return 3-r; }
  /*
  var r = Math.floor((Math.random()*3)+1);
  return r;
  */
}

var allPricePoints = [500,2000,10000];
var currentUtteredPriceSliderIndex;

var allConditions = 
[
[
{"sentenceID":85,"domain":"laptop","modifier":"a","number":"round","utterance":500,"buyer":"Dean"},
{"sentenceID":87,"domain":"laptop","modifier":"a","number":"round","utterance":2000,"buyer":"Rodney"},
{"sentenceID":88,"domain":"laptop","modifier":"a","number":"round","utterance":10000,"buyer":"Will"},
{"sentenceID":93,"domain":"laptop","modifier":"a","number":"sharp","utterance":500,"buyer":"Joseph"},
{"sentenceID":95,"domain":"laptop","modifier":"a","number":"sharp","utterance":2000,"buyer":"Carl"},
{"sentenceID":96,"domain":"laptop","modifier":"a","number":"sharp","utterance":10000,"buyer":"Leon"}
]
];

/*
var debugConditions =
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
{"sentenceID":9,"domain":"electric kettle","modifier":"an","number":"sharp","utterance":20,"buyer":"Ivan"}
]
];
*/

var debug = false;
if(debug) { allConditions = debugConditions; }


var numConditions = allConditions.length;
var chooseCondition = random(0, numConditions-1);
var allTrialOrders = allConditions[chooseCondition];
var numTrials = allTrialOrders.length;
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
	utteredPricesRounded: new Array(numTrials),
  inferredPrices0: new Array(numTrials),
  inferredPrices1: new Array(numTrials),
  inferredPrices2: new Array(numTrials),
  inferredPrices3: new Array(numTrials),
  inferredPrices4: new Array(numTrials),
  inferredPrices5: new Array(numTrials),
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
      //var price = 0;//parseFloat(document.price.score.value) + parseFloat(document.price.score1.value) / 100.00;
      
      var probPrice0 = parseInt(document.getElementById("hiddenSliderValue0").value);
      var probPrice1 = parseInt(document.getElementById("hiddenSliderValue1").value);
      var probPrice2 = parseInt(document.getElementById("hiddenSliderValue2").value);
      var probPrice3 = parseInt(document.getElementById("hiddenSliderValue3").value);
      var probPrice4 = parseInt(document.getElementById("hiddenSliderValue4").value);
      var probPrice5 = parseInt(document.getElementById("hiddenSliderValue5").value);
      
      var probAffect = parseInt(document.getElementById("hiddenSliderValue6").value) / 40.00;
      
      experiment.inferredPrices0[currentTrialNum] = probPrice0;
      experiment.inferredPrices1[currentTrialNum] = probPrice1;
      experiment.inferredPrices2[currentTrialNum] = probPrice2;
      experiment.inferredPrices3[currentTrialNum] = probPrice3;
      experiment.inferredPrices4[currentTrialNum] = probPrice4;
      experiment.inferredPrices5[currentTrialNum] = probPrice5;
      
      
      experiment.affects[currentTrialNum] = probAffect;
      experiment.orders[currentTrialNum] = numComplete;
      experiment.sentenceIDs[currentTrialNum] = trial.sentenceID;
      experiment.domains[currentTrialNum] = trial.domain;
      experiment.buyers[currentTrialNum] = trial.buyer;
      experiment.utteredPrices[currentTrialNum] = document.getElementById("cost").innerHTML;
      experiment.utteredPricesRounded[currentTrialNum] = trial.utterance;
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
      $("#buyer4").html(buyer);
      $("#domain1").html(trial.domain);
      $("#domain2").html(trial.domain);
      $("#domain3").html(trial.domain);
      $("#domain4").html(trial.domain);
      $("#domain5").html(trial.domain);
      $("#domain6").html(trial.domain);
      $("#modifier").html(trial.modifier);
      
      for (var i = 0; i <= 2; i++)
      {
        var j = i*2;
        
        
        $("#cost" + j).html(numberWithCommas(allPricePoints[i]));
        j = j+1;
        $("#cost" + j).html(numberWithCommas(allPricePoints[i] + randomizeSharpOffset()));
      }

      for (var i = 0; i <= 2; i++)
      {
        if ( allPricePoints[i] == trial.utterance )
        {
          currentUtteredPriceSliderIndex = 2*i;
        } 
      }
      
      var currentUtteredPrice;
      if ( trial.number == "round" )
      { currentUtteredPrice = trial.utterance; }
      else
      { 
        currentUtteredPrice = trial.utterance + randomizeSharpOffset();
        currentUtteredPriceSliderIndex = currentUtteredPriceSliderIndex + 1;
      }
      
      
      $("#cost").html(toWords(currentUtteredPrice));
      var utteredSliderIndexName = "#cost" + currentUtteredPriceSliderIndex;
      $(utteredSliderIndexName).html(numberWithCommas(currentUtteredPrice));
      numComplete++;
    }
  }
}

// scripts for sliders
$("#slider0").slider({
               animate: true,
               orientation: "vertical",
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
               }});
$("#slider1").slider({
               animate: true,
               orientation: "vertical",
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider1 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue1').attr('value', ui.value);
                   $("#slider1").css({"background":"#99D6EB"});
                   $("#slider1 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider2").slider({
               animate: true,
               orientation: "vertical",
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider2 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue2').attr('value', ui.value);
                   $("#slider2").css({"background":"#99D6EB"});
                   $("#slider2 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider3").slider({
               animate: true,
               orientation: "vertical",
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider3 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue3').attr('value', ui.value);
                   $("#slider3").css({"background":"#99D6EB"});
                   $("#slider3 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider4").slider({
               animate: true,
               orientation: "vertical",
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider4 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue4').attr('value', ui.value);
                   $("#slider4").css({"background":"#99D6EB"});
                   $("#slider4 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider5").slider({
               animate: true,
               orientation: "vertical",
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider5 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue5').attr('value', ui.value);
                   $("#slider5").css({"background":"#99D6EB"});
                   $("#slider5 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});

$("#slider6").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider6 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue6').attr('value', ui.value);
                   $("#slider6").css({"background":"#99D6EB"});
                   $("#slider6 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});

