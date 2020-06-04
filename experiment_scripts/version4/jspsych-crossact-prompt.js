/**
 * crossact- learning - prompt
 * plugin for learning trials in cross-situational word learning study (production vs. comprehension, crossPC)
 * Martin Zettersten
 */

jsPsych.plugins['crossact-prompt'] = (function() {

  var plugin = {};
  
  var context = new AudioContext();

  plugin.trial = function(display_element, trial) {
	  
      // default values
      trial.canvas_size = trial.canvas_size || [1024,700];
	  trial.standardIm = trial.standardIm || ["stims/Bear_Smile.png"];
	  trial.timing_post_trial = typeof trial.timing_post_trial == 'undefined' ? 0 : trial.timing_post_trial;
	  trial.endTrialPause = trial.endTrialPause || 500;
	  trial.standardIm2 = trial.standardIm2 || ["stims/space_helmet.png"];
	  trial.audio = trial.audio || "stims/bleep.wav";
	  trial.responseKind = trial.responseKind || "click";
	  
      // if any trial variables are functions
      // this evaluates the function and replaces
      // it with the output of the function
      trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);
	  
	//function to play audio
	  function playSound(buffer) {
	    var source = context.createBufferSource(); // creates a sound source
	    source.buffer = jsPsych.pluginAPI.getAudioBuffer(buffer);                    // tell the source which sound to play
	    source.connect(context.destination);       // connect the source to the context's destination (the speakers)
	    source.start(0);                           // play the source now
	  }
	  
	  display_element.append($("<svg id='jspsych-training-canvas' width=" + trial.canvas_size[0] + " height=" + trial.canvas_size[1] + "></svg>"));

      var paper = Snap("#jspsych-training-canvas");
	  
	  var standardBack = paper.image(trial.standardIm,311,225,179,258);
	  var standard2 = paper.image(trial.standardIm2,288,187,230,210);
	  var standard = paper.image(trial.standardIm,311,225,179,258);
	  standard.attr({opacity:0});
	  
	  var rt = "NA";
	  var start_time = (new Date()).getTime();
	    
	  var trial_data={};
	  if (trial.responseKind == "touch") {
		  standard.touchstart(function() {
			  standard.untouchstart();
	  		var end_time = (new Date()).getTime();
	  		rt = end_time - start_time;
			playSound(trial.audio);
			setTimeout(function() {
				endTrial();
			}, trial.endTrialPause);
		});
	  } else {
		  standard.click(function() {
			  standard.unclick();
	  		var end_time = (new Date()).getTime();
	  		rt = end_time - start_time;
			playSound(trial.audio);
			setTimeout(function() {
				endTrial();
			}, trial.endTrialPause);
		});
	  }


		
		


	  
	  
      function endTrial() {
		//var audioFeedback = new Audio(trial.audioFeedback);
		//audioFeedback.play();
        var trial_data = {
			"rt": rt
		};
		display_element.html('');
		jsPsych.finishTrial(trial_data);
		
      };
  };	  
		
		return plugin;
})();