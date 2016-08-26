// Copyright (C) 2014, 2015, 2016 Jan Moringen
//
// Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

function RefreshWidget(api, element, args) {
    var self = this;

    this.api      = api;
    this.interval = 10000;
    this.element  = element;

    this.label    = $('<span class="glyphicon"></span>');
    this.button   = $('<div class="btn btn-default"></div>');
    this.button.on('click', function () { self.toggle(); });
    this.button.append(this.label);
    this.element.append(this.button);
    if (args && args.enabled) {
        this.enabled();
    } else {
        this.disable();
    }
}

RefreshWidget.prototype.enable = function () {
    if (this.timer) {
        clearTimeout(this.timer);
    }
    this.timer = setTimeout(this.refresh, this.interval);
    this.label.removeClass('glyphicon-refresh')
        .addClass('glyphicon-pause');
};

RefreshWidget.prototype.disable = function () {
    if (this.timer) {
        clearTimeout(this.timer);
    }
    this.timer = undefined;
    this.label.removeClass('glyphicon-pause')
        .addClass('glyphicon-refresh');
};

RefreshWidget.prototype.toggle = function () {
    if (this.timer) {
        this.disable();
    } else {
        this.enable();
    }
}
