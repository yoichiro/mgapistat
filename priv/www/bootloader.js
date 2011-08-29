var mgapistat = function() {};

mgapistat.BootLoader = function() {
    this.initialize();
};

mgapistat.BootLoader.prototype = {
    searchConditionPane: null,
    searchResultPane: null,
    initialize: function() {
        this.searchConditionPane = new mgapistat.SearchConditionPane();
        this.searchResultPane = new mgapistat.SearchResultPane();
        this.setupEvents();
	this.onSelectTab(dijit.byId("currentPane"));
    },
    setupEvents: function() {
        dojo.subscribe("changeCondition", this.searchResultPane, "onChangeCondition");
        dojo.subscribe("changeProgress", this, "onChangeProgress");
	dojo.subscribe("changeCurrentStatuses", this.searchResultPane, "onChangeCurrentStatuses");
	dojo.subscribe("showErrorMessage", this, "showErrorMessage");
	dojo.subscribe("centerPanel-selectChild", this, "onSelectTab");
    },
    onSelectTab: function(child) {
	this.searchConditionPane.onSelectTab(child);
	this.searchResultPane.onSelectTab(Child);
    },
    onChangeProgress: function(state) {
        var progress = dojo.byId("progress");
        if (state) {
            dojo.style(progress, "visibility", "visible");
        } else {
            dojo.style(progress, "visibility", "hidden");
        }
    },
    showErrorMessage: function(message) {
	var e = dojo.byId("errorMessage");
	e.innerHTML = message;
	setTimeout(function() {
	    e.innerHTML = "";
	}, 5000);
    }
};

dojo.addOnLoad(function() {
    mgapistat.bootLoader = new mgapistat.BootLoader();
});
