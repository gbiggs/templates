/* RTC<component>
 *
 * Header file for the component.
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
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * RTC<component> is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with RTC<component>. If not, see <http://www.gnu.org/licenses/>.
 */


#if !defined(RTC<component>_H__)
#define RTC<component>_H__

#include <rtm/Manager.h>
#include <rtm/DataFlowComponentBase.h>
#include <rtm/OutPort.h>
#include <rtm/CorbaPort.h>

#include <rtc<component>/<interface>_impl.h>

using namespace RTC;

namespace rtc<component>
{
    class RTC<component>
    : public RTC::DataFlowComponentBase
    {
        public:
            RTC<component>(RTC::Manager* manager);
            ~RTC<component>();

            virtual RTC::ReturnCode_t onInitialize();
            virtual RTC::ReturnCode_t onActivated(RTC::UniqueId ec_id);
            virtual RTC::ReturnCode_t onDeactivated(RTC::UniqueId ec_id);
            virtual RTC::ReturnCode_t onExecute(RTC::UniqueId ec_id);

        protected:
            // Ports
            RTC::<datatype> in_;
            RTC::InPort<RTC::<datatype>> in_port_;
            RTC::<datatype> out_;
            RTC::OutPort<RTC::<datatype>> out_port_;
            <module>::<interface>Provider svc_prov_;
            RTC::CorbaPort svc_port_;

            // Configurable settings
    }; // class RTC<component>
}; // namespace rtc<component>


extern "C"
{
    DLL_EXPORT void rtc_init(RTC::Manager* manager);
};

#endif // RTC<component>_H_

