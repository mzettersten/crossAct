/**
 * crossPC - learning
 * plugin for learning trials in cross-situational word learning study (production vs. comprehension, crossPC)
 * Martin Zettersten
 */

jsPsych.plugins['learning'] = (function() {

  var plugin = {};
  
  var context = new AudioContext();
  
  jsPsych.pluginAPI.registerPreload('learning', 'im1', 'image');
  jsPsych.pluginAPI.registerPreload('learning', 'im2', 'image');
  jsPsych.pluginAPI.registerPreload('learning', 'audio1', 'audio');
  jsPsych.pluginAPI.registerPreload('learning', 'audio2', 'audio');

  plugin.trial = function(display_element, trial) {
	  
      // default values
      trial.canvas_size = trial.canvas_size || [1024,700];
      trial.image_size = trial.image_size || [150, 150];
	  trial.location1 = trial.location1 || "left";
	  trial.location2 = trial.location2 || "right";
	  trial.standardIm = trial.standardIm || ["stims/Bear_Smile.png"];
	  trial.standardImTalk = trial.standardImTalk || ["stims/Bear_Talk.png"];
	  trial.label1 = trial.label1 || "kita";
	  trial.label2 = trial.label2 || "kita";
	  trial.audio1 = trial.audio1 || "kita.m4a";
	  trial.audio2 = trial.audio2 || "kita.m4a";
	  trial.duration = trial.duration || 1000;
	  trial.timing_post_trial = typeof trial.timing_post_trial == 'undefined' ? 0 : trial.timing_post_trial;
	  trial.endTrialPause = trial.endTrialPause || 500;
	  trial.audioDuration = trial.audioDuration || 728;
	  trial.audioPause = trial.audioPause || 1000;
	  trial.standardIm2 = trial.standardIm2 || ["stims/space_helmet.png"];
	  trial.repeat = trial.repeat || "true";
	  trial.responseKind = trial.responseKind || "click";
	  
      // if any trial variables are functions
      // this evaluates the function and replaces
      // it with the output of the function
      trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);
	  
	  display_element.append($("<svg id='jspsych-training-canvas' width=" + trial.canvas_size[0] + " height=" + trial.canvas_size[1] + "></svg>"));

      var paper = Snap("#jspsych-training-canvas");
	  
	  var leftCircle = paper.circle(100, 350, 100);
	  leftCircle.attr({
		  fill: "#9ecae1",
		  stroke: "#000",
		  strokeWidth: 5
	  });
	  
	  var rightCircle = paper.circle(700, 350, 100);
	  rightCircle.attr({
		  fill: "#9ecae1",
		  stroke: "#000",
		  strokeWidth: 5
	  });

	  
	  var imageLocations = {
		  left: [25, 275],
		  right: [625, 275]
	  };
	  
	//function to play audio
	  function playSound(buffer) {
	    var source = context.createBufferSource(); // creates a sound source
	    source.buffer = jsPsych.pluginAPI.getAudioBuffer(buffer);                    // tell the source which sound to play
	    source.connect(context.destination);       // connect the source to the context's destination (the speakers)
	    source.start(0);                           // play the source now
	  }
	  
	  var standardTalk = paper.image(trial.standardImTalk,311,225,179,258);
	  var standardBack = paper.image(trial.standardIm,311,225,179,258);
	  var standard2 = paper.image(trial.standardIm2,288,187,230,210);
	  var standard = paper.image(trial.standardIm,311,225,179,258);
	  standard.attr({opacity:0});
	  
	  
	  var image1 = paper.image(trial.im1, imageLocations[trial.location1][0], imageLocations[trial.location1][1], trial.image_size[0],trial.image_size[1]);
	  var image2 = paper.image(trial.im2, imageLocations[trial.location2][0], imageLocations[trial.location2][1], trial.image_size[0],trial.image_size[1]);
	  
	  var rt = "NA";
	  var start_time = (new Date()).getTime();
	    
	  var trial_data={};
	  
	  if (trial.responseKind=="touch") {
		  standard.touchstart(function() {
  				  inputEvent();
			  });
		  } else {
			  standard.click(function() {
	  				  inputEvent();
				  });
		  }
	
	function inputEvent() {
		var end_time = (new Date()).getTime();
		rt = end_time - start_time;
		if (trial.responseKind=="click") {
			standard.unclick();
		} else {
			standard.untouchstart();
		};
		standardBack.attr({opacity: 0});
		
	 playSound(trial.audio1);
		setTimeout(function() {
			playSound(trial.audio2);
			setTimeout(function() {
				if (trial.repeat == "true") {
					image1.animate({opacity: "0"},250,mina.easeinout, function() {
						image1.animate({x: imageLocations[trial.location2][0],y: imageLocations[trial.location2][1]},0,mina.linear, function() {
							image1.animate({opacity: "1"},250,mina.easeinout);
						});
					});
					
					image2.animate({opacity: "0"},250,mina.easeinout, function() {
						image2.animate({x: imageLocations[trial.location1][0],y: imageLocations[trial.location1][1]},0,mina.linear, function() {
							image2.animate({opacity: "1"},250,mina.easeinout);
						});
					});
					setTimeout(function() {
						playSound(trial.audio1);
						setTimeout(function() {
							playSound(trial.audio2);
							setTimeout(function() {
								endTrial();
							}, trial.audioDuration+trial.endTrialPause);
						},trial.audioDuration+trial.audioPause);	
					}, trial.audioPause);
			   	 
				} else {
					setTimeout(function() {
						endTrial();
					}, trial.endTrialPause);
				}
			}, trial.audioDuration+trial.endTrialPause);
		}, trial.audioDuration+trial.audioPause);
  };

	  
	  
      function endTrial() {
		//var audioFeedback = new Audio(trial.audioFeedback);
		//audioFeedback.play();
        var trial_data = {
			"label1": trial.label1,
			"label2": trial.label2,
			"location1": trial.location1,
			"location2": trial.location2,
			"image1": trial.im1,
			"image2": trial.im2,
			"audio1": trial.audio1,
			"audio2": trial.audio2,
			"rt": rt
		};
		
		display_element.html('');
		jsPsych.finishTrial(trial_data);
		
      };
  };	  
		
		return plugin;
})();