dojo.require("dijit.layout.BorderContainer");
dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.dijit");
dojo.require("dijit.Calendar");
dojo.require("dijit.form.CheckBox");
dojo.require("dojox.charting.Chart2D");
dojo.require("dojox.charting.widget.Legend");
dojo.require("dojox.charting.action2d.Tooltip");
dojo.require("dojox.charting.action2d.Magnify");
dojo.require("dojox.charting.themes.Shrooms");
dojo.require("dojox.grid.DataGrid");
dojo.require("dojo.data.ItemFileWriteStore");

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

//        this.searchConditionPane.notifyConditionChanged();
//	this.searchConditionPane.notifyCurrentStatusesChanged();
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

mgapistat.SearchConditionPane = function() {
    this.initialize();
};

mgapistat.SearchConditionPane.prototype = {
    autoReloadState: true,
    initialize: function() {
        this.setupDatePicker();
        this.setupApiTypes();
	this.setupAutoReload();

	dojo.connect(dojo.byId("reloadStatuses"), "onclick", this, "notifyCurrentStatusesChanged");
    },
    setupDatePicker: function() {
        var datePicker = new dijit.Calendar({
            value: new Date()
        }, "datePicker");
        dojo.connect(datePicker, "onChange", this, "onChangeDatePicker");
    },
    setupApiTypes: function() {
        var self = this;
        for (var i = 1; i <= 9; i++) {
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
        for (var i = 1; i <= 9; i++) {
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
	var tab = this.getSelectedTab();
	if (tab == "Current") {
            dojo.publish("changeCurrentStatuses", [apiTypes]);
	} else if (tab == "History") {
            dojo.publish("changeCondition", [date, apiTypes]);
	}
    },
    notifyCurrentStatusesChanged: function() {
	var apiTypes = this.getSelectedApiTypes();
	dojo.publish("changeCurrentStatuses", [apiTypes]);
    },
    getSelectedTab: function(child) {
	var tabPane = dijit.byId("centerPanel");
	return tabPane.selectedChildWidget.title;
    },
    onSelectTab: function(child) {
	if (child.title == "Current") {
	    dojo.style(dojo.byId("datePicker"), "display", "none");
//	    dojo.style(dojo.byId("apiTypePanel"), "display", "none");
	    dojo.style(dojo.byId("reloadPanel"), "display", "block");
	    this.notifyCurrentStatusesChanged();
	} else if (child.title == "History") {
	    dojo.style(dojo.byId("datePicker"), "display", "block");
//	    dojo.style(dojo.byId("apiTypePanel"), "display", "block");
	    dojo.style(dojo.byId("reloadPanel"), "display", "none");
	    this.notifyConditionChanged();
	}
    }
};

mgapistat.SearchResultPane = function() {
    this.initialize();
};

mgapistat.SearchResultPane.prototype = {
    historyChart: null,
    historyLegend: null,
    recentChart: null,
    recentLegend: null,
    myDataStore: null,
    initialize: function() {
        this.setupHistoryChart();
        this.setupGrid();
	this.setupRecentChart();
    },
    loadCurrentStatuses: function(apiTypes) {
	var self = this;
        dojo.publish("changeProgress", [true]);
        dojo.xhrPost({
            url: "ajax/current",
            handleAs: "json",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded; charset=utf-8"
            },
            postData: dojo.objectToQuery({
                apiTypes: apiTypes.join(",")
            }),
            load: dojo.hitch(self, dojo.hitch(self, function(response) {
                this.renderCurrentStatuses(response);
		dojo.publish("changeProgress", [false]);
            })),
            error: dojo.hitch(self, dojo.hitch(self, function(message) {
                dojo.publish("showErrorMessage", [message]);
		dojo.publish("changeProgress", [false]);
            }))
	});
    },
    removeAllCurrentStatuses: function() {
        this.myDataStore.fetch({query: {apiType: "*"}, onComplete: dojo.hitch(this, function(items) {
            dojo.forEach(items, dojo.hitch(this, function(item) {
                this.myDataStore.deleteItem(item);
            }));
        })});
    },
    renderCurrentStatuses: function(response) {
	var result = response.result;
	this.removeAllCurrentStatuses();
	dojo.forEach(result, dojo.hitch(this, function(item) {
	    var status = "0";
	    if (item.deviation >= 70) {
		status = "2";
	    } else if (item.deviation >= 60 && item.deviation < 70) {
		status = "1";
	    }
	    this.myDataStore.newItem({
		apiType: item.apiType,
		status: status,
		latency: item.recentAverage,
		average: item.average,
		stdDeviation: item.stdDeviation,
		deviation: item.deviation
	    });
	}));
    },
    onChangeCurrentStatuses: function(apiTypes) {
	this.loadCurrentStatuses(apiTypes);
	this.loadRecentLatency(apiTypes);
    },
    setupGrid: function() {
        this.myDataStore = new dojo.data.ItemFileWriteStore({
            data: {items: [
            ]}
        });
        var layout = [
            {field: "status", name: "Status", width: "80px",
                formatter: function(value) {
                    switch(value) {
                        case "0": return "<div class='statusGood'>Good</div>";
                        case "1": return "<div class='statusCaution'>Caution</div>";
			case "2": return "<div class='statusWarning'>Warning</div>";
                        default: return "";
                    }
                }
            },
            {field: "apiType", name: "API type", width: "auto",
                formatter: function(value) {
                    switch(value) {
                        case "1": return "Refresh an access token";
                        case "2": return "people/@me/@self";
                        case "3": return "people/@me/@friends";
                        case "4": return "updates/@me/@friends";
                        case "5": return "voice/statuses/friends_timeline";
			case "6": return "photo/albums/@me/@friends";
			case "7": return "photo/mediaItems/@me/@friends";
			case "8": return "checkins/@me/@friends";
			case "9": return "messages/@me/@inbox";
                        default: return "";
                    }
                }
            },
	    {field: "deviation", name: "Deviation", width: "100px"},
            {field: "latency", name: "Recent latency", width: "160px"},
            {field: "average", name: "Average latency", width: "170px"}
//            {field: "stdDeviation", name: "Std. deviation", width: "auto"}
        ];
        var grid = new dojox.grid.DataGrid({
            id: "grid",
            autoHeight: true,
            store: this.myDataStore,
            structure: layout
        }, "grid");
        grid.startup();

	dojo.connect(grid, "onRowDblClick", this, "onGridRowDblClick");
    },
    onGridRowDblClick: function(evt) {
	console.log(evt);
    },
    setupHistoryChart: function() {
        this.historyChart = new dojox.charting.Chart2D("historyChart");
        this.historyChart.setTheme(dojox.charting.themes.Shrooms);
        this.historyChart.addPlot("default", {
            type: "Lines",
            markers: true
        });
        this.historyChart.addAxis("x", {majorTickStep: 1, minorTickStep: 0});
        this.historyChart.addAxis("y", {min: 0, vertical: true, fixLower: "major", fixUpper: "major"});
        var tip = new dojox.charting.action2d.Tooltip(this.historyChart, "default");
        var mag = new dojox.charting.action2d.Magnify(this.historyChart, "default");
//        this.historyChart.render();
	this.historyChart.resize(700, 300);
        this.historyLegend = new dojox.charting.widget.Legend({ chart: this.historyChart }, "historyLegend");
	this.historyChart.connectToPlot("default", dojo.hitch(this, function(evt) {
	    console.log(evt.type);
	}));
    },
    renderHistoryChart: function(response) {
        var apiTypes = [
            {apiType: "1", legend: "1:Refresh token"},
            {apiType: "2", legend: "2:People (@me)"},
            {apiType: "3", legend: "3:People (@friends)"},
	    {apiType: "4", legend: "4:Updates"},
	    {apiType: "5", legend: "5:Voice"},
	    {apiType: "6", legend: "6:Albums"},
	    {apiType: "7", legend: "7:MediaItems"},
	    {apiType: "8", legend: "8:Check-ins"},
	    {apiType: "9", legend: "9:Messages"}
        ];
        var result = response.result;
        dojo.forEach(apiTypes, dojo.hitch(this, function(item) {
            var source = result[item.apiType];
            if (source) {
                this.historyChart.removeSeries(item.legend);
                if (source.labels.length > 0) {
                    var labels = new Array();
                    dojo.forEach(source.labels, function(label, index, attr) {
                        labels.push({
                            value: index + 1,
                            text: label
                        });
                    });
                    this.historyChart.addAxis("x", {
                        majorTickStep: 1,
                        minorTickStep: 0,
                        labels: labels
                    });
                    this.historyChart.addSeries(item.legend, source.values);
                }
            } else {
                this.historyChart.removeSeries(item.legend);
            }
        }));
        this.historyChart.resize(700, 300);
        this.historyLegend.refresh();
    },
    setupRecentChart: function() {
        this.recentChart = new dojox.charting.Chart2D("recentChart");
        this.recentChart.setTheme(dojox.charting.themes.Shrooms);
        this.recentChart.addPlot("default", {
            type: "Lines",
            markers: true
        });
        this.recentChart.addAxis("x", {majorTickStep: 1, minorTickStep: 0});
        this.recentChart.addAxis("y", {min: 0, vertical: true, fixLower: "major", fixUpper: "major"});
        var tip = new dojox.charting.action2d.Tooltip(this.recentChart, "default");
        var mag = new dojox.charting.action2d.Magnify(this.recentChart, "default");
//        this.recentChart.render();
	this.recentChart.resize(700, 300);
        this.recentLegend = new dojox.charting.widget.Legend({ chart: this.recentChart }, "recentLegend");
	this.recentChart.connectToPlot("default", dojo.hitch(this, function(evt) {
	    console.log(evt.type);
	}));
    },
    renderRecentChart: function(response) {
        var apiTypes = [
            {apiType: "1", legend: "1:Refresh token"},
            {apiType: "2", legend: "2:People (@me)"},
            {apiType: "3", legend: "3:People (@friends)"},
	    {apiType: "4", legend: "4:Updates"},
	    {apiType: "5", legend: "5:Voice"},
	    {apiType: "6", legend: "6:Albums"},
	    {apiType: "7", legend: "7:MediaItems"},
	    {apiType: "8", legend: "8:Check-ins"},
	    {apiType: "9", legend: "9:Messages"}
        ];
        var result = response.result;
        dojo.forEach(apiTypes, dojo.hitch(this, function(item) {
            var source = result[item.apiType];
            if (source) {
                this.recentChart.removeSeries(item.legend);
                if (source.labels.length > 0) {
                    var labels = new Array();
                    dojo.forEach(source.labels, function(label, index, attr) {
                        labels.push({
                            value: index + 1,
                            text: label
                        });
                    });
                    this.recentChart.addAxis("x", {
                        majorTickStep: 1,
                        minorTickStep: 0,
                        labels: labels
                    });
                    this.recentChart.addSeries(item.legend, source.values);
                }
            } else {
                this.recentChart.removeSeries(item.legend);
            }
        }));
        this.recentChart.resize(700, 300);
        this.recentLegend.refresh();
    },
    loadRecentLatency: function(apiTypes) {
	var self = this;
        dojo.publish("changeProgress", [true]);
        dojo.xhrPost({
            url: "ajax/recent",
            handleAs: "json",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded; charset=utf-8"
            },
            postData: dojo.objectToQuery({
                apiTypes: apiTypes.join(",")
            }),
            load: dojo.hitch(self, dojo.hitch(self, function(response) {
                this.renderRecentChart(response);
		dojo.publish("changeProgress", [false]);
            })),
            error: dojo.hitch(self, dojo.hitch(self, function(message) {
                dojo.publish("showErrorMessage", [message]);
		dojo.publish("changeProgress", [false]);
            }))
	});
    },
    onChangeCondition: function(date, apiTypes) {
	this.loadHistories(date, apiTypes);
    },
    loadHistories: function(date, apiTypes) {
        var self = this;
        dojo.publish("changeProgress", [true]);
        var selectedDate = dojo.date.locale.format(date, {
            fullYear: true,
            datePattern: "yyyy-MM-dd",
            selector: "date"
        });
        dojo.xhrPost({
            url: "ajax/search",
            handleAs: "json",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded; charset=utf-8"
            },
            postData: dojo.objectToQuery({
                date: selectedDate,
                apiTypes: apiTypes.join(",")
            }),
            load: dojo.hitch(self, function(response) {
                this.renderHistoryChart(response);
                dojo.publish("changeProgress", [false]);
            }),
            error: dojo.hitch(self, function(message) {
                dojo.publish("showErrorMessage", [message]);
                dojo.publish("changeProgress", [false]);
            })
        });
    },
    onSelectTab: function(child) {
	if (child.title == "History") {
	    this.historyChart.resize(700, 300);
	} else if (child.title == "Current") {
	    this.recentChart.resize(700, 300);
	}
    }
};

dojo.addOnLoad(function() {
    mgapistat.bootLoader = new mgapistat.BootLoader();
});
