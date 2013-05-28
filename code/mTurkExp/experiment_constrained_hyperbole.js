var NUM_SLIDERS = 17;
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
}

var allPricePoints = [20,50,100,200,500,1000,2000,10000];
var currentUtteredPriceSliderIndex;

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
  inferredPrices0: new Array(numTrials),
  inferredPrices1: new Array(numTrials),
  inferredPrices2: new Array(numTrials),
  inferredPrices3: new Array(numTrials),
  inferredPrices4: new Array(numTrials),
  inferredPrices5: new Array(numTrials),
  inferredPrices6: new Array(numTrials),
  inferredPrices7: new Array(numTrials),
  inferredPrices8: new Array(numTrials),
  inferredPrices9: new Array(numTrials),
  inferredPrices10: new Array(numTrials),
  inferredPrices11: new Array(numTrials),
  inferredPrices12: new Array(numTrials),
  inferredPrices13: new Array(numTrials),
  inferredPrices14: new Array(numTrials),
  inferredPrices15: new Array(numTrials),
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
      var probPrice6 = parseInt(document.getElementById("hiddenSliderValue6").value);
      var probPrice7 = parseInt(document.getElementById("hiddenSliderValue7").value);
      var probPrice8 = parseInt(document.getElementById("hiddenSliderValue8").value);
      var probPrice9 = parseInt(document.getElementById("hiddenSliderValue9").value);
      var probPrice10 = parseInt(document.getElementById("hiddenSliderValue10").value);
      var probPrice11 = parseInt(document.getElementById("hiddenSliderValue11").value);
      var probPrice12 = parseInt(document.getElementById("hiddenSliderValue12").value);
      var probPrice13 = parseInt(document.getElementById("hiddenSliderValue13").value);
      var probPrice14 = parseInt(document.getElementById("hiddenSliderValue14").value);
      var probPrice15 = parseInt(document.getElementById("hiddenSliderValue15").value);
      
    
      var probAffect = parseInt(document.getElementById("hiddenSliderValue16").value) / 40.00;
      
      experiment.inferredPrices0[currentTrialNum] = probPrice0;
      experiment.inferredPrices1[currentTrialNum] = probPrice1;
      experiment.inferredPrices2[currentTrialNum] = probPrice2;
      experiment.inferredPrices3[currentTrialNum] = probPrice3;
      experiment.inferredPrices4[currentTrialNum] = probPrice4;
      experiment.inferredPrices5[currentTrialNum] = probPrice5;
      experiment.inferredPrices6[currentTrialNum] = probPrice6;
      experiment.inferredPrices7[currentTrialNum] = probPrice7;
      experiment.inferredPrices8[currentTrialNum] = probPrice8;
      experiment.inferredPrices9[currentTrialNum] = probPrice9;
      experiment.inferredPrices10[currentTrialNum] = probPrice10;
      experiment.inferredPrices11[currentTrialNum] = probPrice11;
      experiment.inferredPrices12[currentTrialNum] = probPrice12;
      experiment.inferredPrices13[currentTrialNum] = probPrice13;
      experiment.inferredPrices14[currentTrialNum] = probPrice14;
      experiment.inferredPrices15[currentTrialNum] = probPrice15;
      
      
      experiment.affects[currentTrialNum] = probAffect;
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
      $("#domain4").html(trial.domain);
      $("#domain5").html(trial.domain);
      $("#modifier").html(trial.modifier);
      
      for (var i = 0; i <= 7; i++)
      {
        var j = i*2;
        $("#cost" + j).html(allPricePoints[i]);
        j = j+1;
        $("#cost" + j).html(allPricePoints[i] + randomizeSharpOffset());
      }

      for (var i = 0; i <= 7; i++)
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
      
      
      $("#cost").html(currentUtteredPrice);
      var utteredSliderIndexName = "#cost" + currentUtteredPriceSliderIndex;
      $(utteredSliderIndexName).html(currentUtteredPrice);
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
               }});
$("#slider1").slider({
               animate: true,
               
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
$("#slider7").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider7 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue7').attr('value', ui.value);
                   $("#slider7").css({"background":"#99D6EB"});
                   $("#slider7 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider8").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider8 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue8').attr('value', ui.value);
                   $("#slider8").css({"background":"#99D6EB"});
                   $("#slider8 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider9").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider9 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue9').attr('value', ui.value);
                   $("#slider9").css({"background":"#99D6EB"});
                   $("#slider9 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider10").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider10 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue10').attr('value', ui.value);
                   $("#slider10").css({"background":"#99D6EB"});
                   $("#slider10 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider11").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider11 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue11').attr('value', ui.value);
                   $("#slider11").css({"background":"#99D6EB"});
                   $("#slider11 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider12").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider12 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue12').attr('value', ui.value);
                   $("#slider12").css({"background":"#99D6EB"});
                   $("#slider12 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider13").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider13 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue13').attr('value', ui.value);
                   $("#slider13").css({"background":"#99D6EB"});
                   $("#slider13 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider14").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider14 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue14').attr('value', ui.value);
                   $("#slider14").css({"background":"#99D6EB"});
                   $("#slider14 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider15").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider15 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue15').attr('value', ui.value);
                   $("#slider15").css({"background":"#99D6EB"});
                   $("#slider15 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider16").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider16 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue16').attr('value', ui.value);
                   $("#slider16").css({"background":"#99D6EB"});
                   $("#slider16 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});















