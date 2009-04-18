// qs_score - Quicksilver Score
// 
// A port of the Quicksilver string ranking algorithm
// 
// "hello world".score("axl") //=> 0.0
// "hello world".score("ow") //=> 0.6
// "hello world".score("hello world") //=> 1.0
String.prototype.score = function(abbreviation,offset) {
  offset = offset || 0 // TODO: I think this is unused... remove
 
  if(abbreviation.length == 0) return 0.9
  if(abbreviation.length > this.length) return 0.0

  for (var i = abbreviation.length; i > 0; i--) {
	var sub_abbreviation = abbreviation.substring(0,i)
	var index = this.indexOf(sub_abbreviation)


	if(index < 0) continue;
	if(index + abbreviation.length > this.length + offset) continue;

	var next_string		  = this.substring(index+sub_abbreviation.length)
	var next_abbreviation = null

	if(i >= abbreviation.length)
	  next_abbreviation = ''
	else
	  next_abbreviation = abbreviation.substring(i)
 
	var remaining_score	  = next_string.score(next_abbreviation,offset+index)
 
	if (remaining_score > 0) {
	  var score = this.length-next_string.length;

	  if(index != 0) {
		var j = 0;

		var c = this.charCodeAt(index-1)
		if(c==32 || c == 9) {
		  for(var j=(index-2); j >= 0; j--) {
			c = this.charCodeAt(j)
			score -= ((c == 32 || c == 9) ? 1 : 0.15)
		  }
		} else {
		  score -= index
		}
	  }
   
	  score += remaining_score * next_string.length
	  score /= this.length;
	  return score
	}
  }
  return 0.0
}

jQuery.fn.listfilter = function(options)
{
	list = jQuery(options.list);

	if(list.length) 
	{

		var rows = list.children('li')
		var cache = rows.map(function()
		{
			return $('span.title', $(this)).text().toLowerCase();
		});
			
		this.keyup(filter).keyup().parents('form').submit(function()
		{
			return false;
		});
	}
		
	return this;
		
	function filter()
	{
		var term = jQuery.trim(jQuery(this).val().toLowerCase()), scores = [];
		
		if(!term) 
		{
			rows.show();
		}
		else
		{
			rows.hide();

			cache.each(function(i)
			{
				var score = this.score(term);
				if(score > 0)
				{
					scores.push([score, i]); 
				}
			});

			jQuery.each(scores.sort(function(a, b){return b[0] - a[0];}), function()
			{
				jQuery(rows[ this[1] ]).show();
			});
		}
	}
};