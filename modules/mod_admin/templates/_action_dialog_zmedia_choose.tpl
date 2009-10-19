{% tabs id=#tabs %}
<div id="{{ #tabs }}">
	<ul class="clearfix">
		<li><a href="#{{ #tab }}-media">Media on this page</a></li>
		<li><a href="#{{ #tab }}-search">Search other media</a></li>
	</ul>

	<div id="{{ #tab }}-media">
		<p>Choose a media item from this page to insert in the body text.</p>

		{% with m.rsc[id].o.depiction as ids %}
		{% include "_choose_media.tpl" %}
		{% endwith %}

    </div>

	<div id="{{ #tab }}-search">
		<div class="form-item clearfix">
			<label for="media-search">Search for media</label>
			<input id="media-search" />
		</div>	
    </div>
