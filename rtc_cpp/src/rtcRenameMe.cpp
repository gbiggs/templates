/* RTC<component>
 *
 * Component implementation.
 *
 * Copyright 2010-2011 Geoffrey Biggs geoffrey.biggs@aist.go.jp
 *     RT-Synthesis Research Group
 *     Intelligent Systems Research Institute,
 *     National Institute of Advanced Industrial Science and Technology (AIST),
 *     Japan
 *     All rights reserved.
 *
 * This file is part of RTC<component>.
 *
 * RTC<component> is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License,
 * or (at your option) any later version.
 *
 * RTC<component> is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with RTC<component>. If not, see <http://www.gnu.org/licenses/>.
 */


#include <rtc<component>/rtc<component>.h>


RTC<component>::RTC<component>(RTC::Manager* manager)
    : RTC::DataFlowComponentBase(manager),
    in_port_("in", in_),
    out_port_("out", out_),
    svc_prov_(),
    svc_port_("svc"),
{
}


RTC<component>::~RTC<component>()
{
}


RTC::ReturnCode_t RTC<component>::onInitialize()
{
    //bindParameter("param", param_, "param_value");

    addInPort(in_port_.getName(), in_port_);
    addOutPort(out_port_.getName(), out_port_);
    svc_port_.registerProvider("svc", "Svc", svc_prov_);
    addPort(svc_port_);
    svc_prov_.setup(this);

    return RTC::RTC_OK;
}


RTC::ReturnCode_t RTC<component>::onActivated(RTC::UniqueId ec_id)
{
    return RTC::RTC_OK;
}


RTC::ReturnCode_t RTC<component>::onDeactivated(RTC::UniqueId ec_id)
{
    return RTC::RTC_OK;
}


RTC::ReturnCode_t RTC<component>::onExecute(RTC::UniqueId ec_id)
{
    return RTC::RTC_OK;
}


extern "C"
{
    void rtc_init(RTC::Manager* manager)
    {
        coil::Properties profile(spec);
        manager->registerFactory(profile, RTC::Create<RTC<component>>,
                RTC::Delete<RTC<component>>);
    }
};

