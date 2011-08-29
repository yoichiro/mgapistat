mgapistat.SearchConditionPane = function() {
    this.initialize();
};

mgapistat.SearchConditionPane.prototype = {
    autoReloadState: true,
    recentLatencySpans: new Array(),
    historyTypes: new Array(),

    initialize: function() {
        this.setupDatePicker();
        this.setupApiTypes();
	this.setupAutoReload();
	this.setupRecentLatencySpan();
	this.setupHistoryType();

	dojo.connect(dojo.byId("reloadStatuses"), "onclick", this, "notifyCurrentStatusesChanged");
    },
    setupRecentLatencySpan: function() {
	var self = this;
	var spans = ["65", "125"];
	dojo.forEach(spans, function(item, index, attr) {
	    self.recentLatencySpans.push(new dijit.form.RadioButton({
		checked: index == 0,
		value: item,
		name: "recentLatencySpan"
	    }, "rls" + (index + 1)));
	    dojo.connect(dijit.byId("rls" + (index + 1)), "onClick", self, function(evt) {
		self.notifyCurrentStatusesChanged();
	    });
	});
    },
    getSelectedRecentLatencySpan: function() {
	for (var i = 0; i < this.recentLatencySpans.length; i++) {
	    if (this.recentLatencySpans[i].checked) {
		return this.recentLatencySpans[i].value;
	    }
	}
	return null;
    },
    setupHistoryType: function() {
	var self = this;
	var types = ["avg", "max"];
	dojo.forEach(types, function(item, index, attr) {
	    self.historyTypes.push(new dijit.form.RadioButton({
		checked: index == 0,
		value: item,
		name: "historyType"
	    }, "ht" + (index + 1)));
	    dojo.connect(dijit.byId("ht" + (index + 1)), "onClick", self, function(evt) {
		self.notifyConditionChanged();
	    });
	});
    },
    getHistoryType: function() {
	for (var i = 0; i < this.historyTypes.length; i++) {
	    if (this.historyTypes[i].checked) {
		return this.historyTypes[i].value;
	    }
	}
	return null;
    },
    setupDatePicker: function() {
        var datePicker = new dijit.Calendar({
            value: new Date()
        }, "datePicker");
        dojo.connect(datePicker, "onChange", this, "onChangeDatePicker");
    },
    setupApiTypes: function() {
        var self = this;
        for (var i = 1; i <= 10; i++) {
            new dijit.form.CheckBox({
                name: "api_type" + i,
                value: i,
                checked: true,
                onChange: dojo.hitch(self, function(state) {
                    this.onChangeApiType(state);
                })
            }, "api_type" + i);
        }
    },
    setupAutoReload: function() {
	var self = this;
	new dijit.form.CheckBox({
	    checked: true,
	    onChange: dojo.hitch(self, function(state) {
		this.autoReloadState = state;
	    })
	}, "auto_reload");
	setTimeout(dojo.hitch(this, function() {
	    this.onTimerAutoReload();
	}), 60000);
    },
    onTimerAutoReload: function() {
	if (this.autoReloadState) {
	    this.notifyConditionChanged();
	    this.notifyCurrentStatusesChanged();
	}
	setTimeout(dojo.hitch(this, function() {
	    this.onTimerAutoReload();
	}), 60000);
    },
    onChangeDatePicker: function(date) {
        this.notifyConditionChanged();
    },
    onChangeApiType: function(state) {
        this.notifyConditionChanged();
    },
    getSelectedApiTypes: function() {
        var apiTypes = new Array();
        for (var i = 1; i <= 10; i++) {
            var apiType = dijit.byId("api_type" + i);
            if (apiType.checked) {
                apiTypes.push(i);
            }
        }
	return apiTypes;
    },
    notifyConditionChanged: function() {
        var datePicker = dijit.byId("datePicker");
        var date = datePicker.value;
        var apiTypes = this.getSelectedApiTypes();
	var recentLatencySpan = this.getSelectedRecentLatencySpan();
	var historyType = this.getHistoryType();
	var tab = this.getSelectedTab();
	if (tab == "Recent") {
            dojo.publish("changeCurrentStatuses", [apiTypes, recentLatencySpan]);
	} else if (tab == "History") {
            dojo.publish("changeCondition", [date, apiTypes, historyType]);
	}
    },
    notifyCurrentStatusesChanged: function() {
	var apiTypes = this.getSelectedApiTypes();
	var recentLatencySpan = this.getSelectedRecentLatencySpan();
	dojo.publish("changeCurrentStatuses", [apiTypes, recentLatencySpan]);
    },
    getSelectedTab: function(child) {
	var tabPane = dijit.byId("centerPanel");
	return tabPane.selectedChildWidget.title;
    },
    onSelectTab: function(child) {
	if (child.title == "Recent") {
	    dojo.style(dojo.byId("datePicker"), "display", "none");
	    dojo.style(dojo.byId("reloadPanel"), "display", "block");
	    dojo.style(dojo.byId("recentLatencySpanPanel"), "display", "block");
	    dojo.style(dojo.byId("historyTypePanel"), "display", "none");
	    this.notifyCurrentStatusesChanged();
	} else if (child.title == "History") {
	    dojo.style(dojo.byId("datePicker"), "display", "block");
	    dojo.style(dojo.byId("reloadPanel"), "display", "none");
	    dojo.style(dojo.byId("recentLatencySpanPanel"), "display", "none");
	    dojo.style(dojo.byId("historyTypePanel"), "display", "block");
	    this.notifyConditionChanged();
	}
    }
};
