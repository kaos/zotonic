{% extends "admin_base.tpl" %}

{% block title %} admin referrers of {{ m.rsc[q.id].title }} {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Referrers to “{{ m.rsc[q.id].title }}”</h2>
			
		{% with m.search.paged[{referrers id=q.id page=q.page}] as result %}

			{% ifequal result.total 0 %}
				<p>There are no pages with a connection to the page “{{ m.rsc[q.id].title }}”</p>
			{% else %}
				<p>The following {% ifequal result.total 1 %}page has{% else %}{{ result.total }} pages have{% endifequal %} a connection to the page “{{ m.rsc[q.id].title }}”.</p>
			{% endifequal %}

			{% pager result=result dispatch="admin_referrers" id=q.id qargs %}
			
			<h3 class="above-list">Referrers</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-30">Title</span>
					<span class="zp-15">Predicate</span>
					<span class="zp-15">Category</span>
					<span class="zp-15">Modified on</span>
					<span class="zp-15">Modified by</span>
					<span class="zp-10">Options</span>
				</li>
			{% for id, pred_id in result %}
				<li id="{{ #li.id }}">
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						<span class="zp-30">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
						<span class="zp-15">{{ m.predicate[pred_id].title }}</span>
						<span class="zp-15">{{ m.rsc[id].category.name }}</span>
						<span class="zp-15">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
						<span class="zp-15">{{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}</span>
						<span class="zp-10">
							{% button text="delete" 
									action={dialog_delete_rsc id=id on_success={slide_fade_out target=#li.id}} %}
							{% button text="edit &raquo;" action={redirect dispatch="admin_edit_rsc" id=id} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					No referrers found
				</li>
			{% endfor %}
			</ul>

			{% pager result=result dispatch="admin_referrers" id=q.id qargs %}

		{% endwith %}

			{#<form method="get" autocomplete="off">
				<input type="text" value="" name="q" id="q" class="do_listfilter {list: '#posts'}" />
			</form>

			<ul id="posts">
				<li><span class="title">The Well-Designed Web</span></li>
				<li><span class="title">Welcome John Nunemaker</span></li> 
				<li><span class="title">Sidebar Creative: The Next Steps</span></li> 
				<li><span class="title">The Web/Desktop Divide</span></li> 
				<li><span class="title">2007 in Review</span></li> 
				<li><span class="title">Don't Complicate the Solution</span></li> 
				<li><span class="title">Blog to Business</span></li> 
				<li><span class="title">Single Line CSS</span></li> 
				<li><span class="title">Comments Work Again</span></li> 
				<li><span class="title">The iPhone Effect</span></li> 
				<li><span class="title">Greek Blogger Camp</span></li> 
				<li><span class="title">FeedBurner FeedSmith</span></li> 
				<li><span class="title">Download Counter Update 1.3</span></li> 
				<li><span class="title">Branding Reworked</span></li>
			</ul>#}
		</div>
		<div class="push"></div>
	</div>
{% endblock %}