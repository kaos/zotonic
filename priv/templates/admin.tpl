{% extends "admin_base.tpl" %}

{% block title %} admin {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Zophrenic Dashboard</h2>

			<div class="zp-50">
				<div class="padding">
					<h3 class="alt">Quick navigation</h3>

					<div class="clearfix">
						<a class="button" href="/admin/overview">manage pages &raquo;</a>
						<a class="button" href="/pages">manage categories &raquo;</a>
						<a class="button" href="/pages">manage prodcuts &raquo;</a>
						<a class="button" href="/pages">manage users &raquo;</a>
					</div>
				</div>
			</div>

			<div class="zp-50">
				<div class="padding">
					<h3 class="alt">Quick search</h3>

					<div  id="quick-search">	
						<form method="get" action="">
							<fieldset>
								<div class="form-element">
									<input type="text" name="q" value="" class="left" />
									<button>Search</button>
								</div>
							</fieldset>
						</form>
					</div>	
				</div>
			</div>

			<hr class="clear" />

			<div class="zp-50">
				<div class="padding">

					<div id="dashboard-pages">
						<h3 class="above-list">Last modified pages</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-25">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-25">Publish date</span>
								<span class="zp-25">Options</span>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">Home</span>
									<span class="zp-25">Struijk</span>
									<span class="zp-25">March 10, 19:33</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">About</span>
									<span class="zp-25">About us</span>
									<span class="zp-25">March 10, 19:38</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">Contact</span>
									<span class="zp-25">About us</span>
									<span class="zp-25">March 10, 20:13</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">About</span>
									<span class="zp-25">About us</span>
									<span class="zp-25">March 10, 19:38</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">Contact</span>
									<span class="zp-25">About us</span>
									<span class="zp-25">March 10, 20:13</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
						</ul>
					</div>
					
					<div id="dashboard-pages">
						<h3 class="above-list">Last modified products</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-25">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-25">Publish date</span>
								<span class="zp-25">Options</span>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">Home</span>
									<span class="zp-25">Struijk</span>
									<span class="zp-25">March 10, 19:33</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">About</span>
									<span class="zp-25">About us</span>
									<span class="zp-25">March 10, 19:38</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">Contact</span>
									<span class="zp-25">About us</span>
									<span class="zp-25">March 10, 20:13</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">About</span>
									<span class="zp-25">About us</span>
									<span class="zp-25">March 10, 19:38</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-25">Contact</span>
									<span class="zp-25">About us</span>
									<span class="zp-25">March 10, 20:13</span>
									<span class="zp-25"><button>edit</button><button>view &raquo;</button></span>
								</a>
							</li>
						</ul>
					</div>
				</div>
			</div>

			<div class="zp-50">
				<div class="padding last">
					<div id="dashboard-products">
						<h3 class="above-list">Best selling products in the last two weeks</h3>
						<img src="http://chart.apis.google.com/chart?cht=p3&amp;chd=t:60,20,20&amp;chs=400x200&amp;chl=Hello|World|Internet" alt="" />
					</div>

					<div id="dashboard-orders">
						<h3 class="above-list">Latest sold products</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-33">Item(s)</span>
								<span class="zp-33">Price</span>
								<span class="zp-33">Date</span>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-33">Shimano v-brake</span>
									<span class="zp-33">&euro; 22,-</span>
									<span class="zp-33">March 15, 22:56</span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-33">Shimano v-brake</span>
									<span class="zp-33">&euro; 22,-</span>
									<span class="zp-33">March 15, 22:58</span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-33">Shimano v-brake</span>
									<span class="zp-33">&euro; 22,-</span>
									<span class="zp-33">March 15, 22:56</span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-33">Shimano v-brake</span>
									<span class="zp-33">&euro; 22,-</span>
									<span class="zp-33">March 15, 22:58</span>
								</a>
							</li>
						</ul>
					</div>
				</div>
			</div>
		</div>
		<div class="push"></div>
	</div>
{% endblock %}