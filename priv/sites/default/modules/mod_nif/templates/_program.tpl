{# The program, optionally filtered #}

{% wire id=#form type="submit" postback={search day=day} delegate="resource_program" %}
<form id="{{ #form }}" method="POST" action="/postback">

<div class="program-filter-wrapper" style="padding: 6px;">
	<h5>Please use these tools to filter the program by day and genre.</h5>
	
	<style type="text/css">
		.day10 #d10, .day11 #d11, .day12 #d12, .day13 #d13,
		.day17 #d17, .day18 #d18, .day19 #d19, .day20 #d20
			{ font-weight: bold; }
	</style>
	
	<div class="program-day-wrapper clearfix day{{ day }}">
		<p>
			{% button type="submit" id="d10" text="Thursday, Sept. 10" %}
			{% button type="submit" id="d11" text="Friday, Sept. 11" %}
			{% button type="submit" id="d12" text="Saturday, Sept. 12" %}
			{% button type="submit" id="d13" text="Sunday, Sept. 13" %}
		</p>
		<p class="clear">
			{% button type="submit" id="d17" text="Thursday, Sept. 17" %}
			{% button type="submit" id="d18" text="Friday, Sept. 18" %}
			{% button type="submit" id="d19" text="Saturday, Sept. 19" %}
			{% button type="submit" id="d20" text="Sunday, Sept. 20" %}
		</p>
	</div>

	<div class="program-day-wrapper clearfix day{{ day }}">
	</div>

	<div class="program-genres-wrapper clearfix">
		<h5>
		{% button text="Show All Genres" postback={show_all day=day} delegate="resource_program" %} or show only:
		</h5>
		
		<div class="clear">
		{% for title, id in m.search[{all_bytitle cat="genre"}] %}
			<div class="form-item left">
				<label>
					<input id="{{ #cat.id }}" type="checkbox" name="{{ id }}" value="1" 
						{% if id|member:genre %}checked="checked"{% endif %} />
					{{ title }}
				</label>
			</div>
			{% wire id=#cat.id type="change" action={submit id=#form} %}
		{% endfor %}
		</div>

	</div>

	{#<div>
		<input id="filter-list" type="text" name="" style="width: 571px;" value="filter the program list by typing the name of a show." />
	</div>
	#}
</div>	

</form>

{% with m.search[{nif_program day=day genre=genre pagelen=1000}] as result %}
	<div class="list-headers">
		<span class="zp-55">Performance or Event</span>
		<span class="zp-25">Time</span>
		<span class="zp-20">Artist</span>
	</div>
	<ul class="program-list clear clearfix">
		{% for id, date_start in result %}

			<li class="clearfix performance-info-wrapper {% cycle 'even' 'uneven' %}">
				<span class="image zp-15">{% image m.rsc[id].media[1] width=80 height=30 crop %}</span>
				
				<span class="artist zp-40">
					<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a>
					{% for genre in m.rsc[id].hasgenre %}
						{% ifequal genre.name "free_event" %}
							<br />{{ genre.title }}
						{% endifequal %}
					{% endfor %}
				</span>
			
				{% if m.rsc[id].date_start %}
					{% with m.rsc[id].date_start as date_start %}
						{% with m.rsc[id].date_end as date_end %}
							<span class="time-wrapper zp-25">
								<span class="time">{{ date_start|date:"f A" }}</span>

								{% ifnotequal date_start date_end %}
									&mdash; <span class="time">{{ date_end|date:"f A" }}</span>
								{% endifnotequal %}
							</span>
							<span class="venue zp-20"><a href="{{ m.rsc[id].o.performer.page_url }}" title="{{ m.rsc[id].o.performer.title }}">{{ m.rsc[id].o.performer.title }}</a></span>
						{% endwith %}
					{% endwith %}
				{% endif %}
			</li>	
		{% empty %}
			<li>
				<p>Sorry, this day there are no events with the selected genres.</p>
			</li>
		{% endfor %}

	</ul>
{% endwith %}


