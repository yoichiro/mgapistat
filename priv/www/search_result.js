mgapistat.SearchResultPane = function() {
    this.initialize();
};

mgapistat.SearchResultPane.prototype = {
    historyChart: null,
    historyLegend: null,
    recentChart: null,
    recentLegend: null,
    timeslotChart: null,
    timeslotLegend: null,
    myDataStore: null,

    initialize: function() {
        this.setupHistoryChart();
        this.setupGrid();
	this.setupRecentChart();
	this.setupTimeslotChart();
    },

// Public methods

    onChangeCondition: function(date, apiTypes, historyType) {
	this.loadHistories(date, apiTypes, historyType);
	this.loadTimeslot(apiTypes, historyType);
    },
    onChangeCurrentStatuses: function(apiTypes, recentLatencySpan) {
	this.loadCurrentStatuses(apiTypes);
	this.loadRecentLatency(apiTypes, recentLatencySpan);
    },
    onSelectTab: function(child) {
	if (child.title == "History") {
	    this.historyChart.resize(700, 300);
	    this.timeslotChart.resize(700, 300);
	} else if (child.title == "Recent") {
	    this.recentChart.resize(700, 300);
	}
    },

// Latency status

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
			case "10": return "Facebook /me";
                        default: return "";
                    }
                }
            },
	    {field: "deviation", name: "Deviation", width: "100px"},
            {field: "latency", name: "Recent latency", width: "160px"},
            {field: "average", name: "Average latency", width: "170px"}
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

// Recent latency Chart

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
	    {apiType: "9", legend: "9:Messages"},
	    {apiType: "10", legend: "10:Facebook"}
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
    loadRecentLatency: function(apiTypes, recentLatencySpan) {
	var self = this;
        dojo.publish("changeProgress", [true]);
        dojo.xhrPost({
            url: "ajax/recent",
            handleAs: "json",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded; charset=utf-8"
            },
            postData: dojo.objectToQuery({
                apiTypes: apiTypes.join(","),
		recentLatencySpan: recentLatencySpan
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

// Average latency Chart

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
	    {apiType: "9", legend: "9:Messages"},
	    {apiType: "10", legend: "10:Facebook"}
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
    loadHistories: function(date, apiTypes, historyType) {
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
                apiTypes: apiTypes.join(","),
		historyType: historyType
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

// Average latency timeslot Chart

    setupTimeslotChart: function() {
        this.timeslotChart = new dojox.charting.Chart2D("timeslotChart");
        this.timeslotChart.setTheme(dojox.charting.themes.Shrooms);
        this.timeslotChart.addPlot("default", {
            type: "Lines",
            markers: true
        });
        this.timeslotChart.addAxis("x", {majorTickStep: 1, minorTickStep: 0});
        this.timeslotChart.addAxis("y", {min: 0, vertical: true, fixLower: "major", fixUpper: "major"});
        var tip = new dojox.charting.action2d.Tooltip(this.timeslotChart, "default");
        var mag = new dojox.charting.action2d.Magnify(this.timeslotChart, "default");
	this.timeslotChart.resize(700, 300);
        this.timeslotLegend = new dojox.charting.widget.Legend({ chart: this.timeslotChart }, "timeslotLegend");
	this.timeslotChart.connectToPlot("default", dojo.hitch(this, function(evt) {
	    console.log(evt.type);
	}));
    },
    renderTimeslotChart: function(response) {
        var apiTypes = [
            {apiType: "1", legend: "1:Refresh token"},
            {apiType: "2", legend: "2:People (@me)"},
            {apiType: "3", legend: "3:People (@friends)"},
	    {apiType: "4", legend: "4:Updates"},
	    {apiType: "5", legend: "5:Voice"},
	    {apiType: "6", legend: "6:Albums"},
	    {apiType: "7", legend: "7:MediaItems"},
	    {apiType: "8", legend: "8:Check-ins"},
	    {apiType: "9", legend: "9:Messages"},
	    {apiType: "10", legend: "10:Facebook"}
        ];
        var result = response.result;
        dojo.forEach(apiTypes, dojo.hitch(this, function(item) {
            var source = result[item.apiType];
            if (source) {
                this.timeslotChart.removeSeries(item.legend);
                if (source.labels.length > 0) {
                    var labels = new Array();
                    dojo.forEach(source.labels, function(label, index, attr) {
                        labels.push({
                            value: index + 1,
                            text: label
                        });
                    });
                    this.timeslotChart.addAxis("x", {
                        majorTickStep: 1,
                        minorTickStep: 0,
                        labels: labels
                    });
                    this.timeslotChart.addSeries(item.legend, source.values);
                }
            } else {
                this.timeslotChart.removeSeries(item.legend);
            }
        }));
        this.timeslotChart.resize(700, 300);
        this.timeslotLegend.refresh();
    },
    loadTimeslot: function(apiTypes, historyType) {
        var self = this;
        dojo.publish("changeProgress", [true]);
        dojo.xhrPost({
            url: "ajax/timeslot",
            handleAs: "json",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded; charset=utf-8"
            },
            postData: dojo.objectToQuery({
                apiTypes: apiTypes.join(","),
		historyType: historyType
            }),
            load: dojo.hitch(self, function(response) {
                this.renderTimeslotChart(response);
                dojo.publish("changeProgress", [false]);
            }),
            error: dojo.hitch(self, function(message) {
                dojo.publish("showErrorMessage", [message]);
                dojo.publish("changeProgress", [false]);
            })
        });
    }

};
