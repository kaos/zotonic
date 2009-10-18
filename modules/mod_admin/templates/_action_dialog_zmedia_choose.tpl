
{% tabs id=#tabs %}
<div id="{{ #tabs }}">
	<ul>
		<li><a href="#{{ #tab }}-media">Media on this page</a></li>
		<li><a href="#{{ #tab }}-search">Search other media</a></li>
	</ul>

	<div id="{{ #tab }}-media">

        <p>Choose a media item from this page to insert in the body text.</p>

        {% with m.rsc[id].o.depiction as ids %}
        {% include "_choose_media.tpl" %}
        {% endwith %}

    </div>

	<div id="{{ #tab }}-media">
        Search: <input />
    </div>

{% button text="Cancel" action={dialog_close} %}
